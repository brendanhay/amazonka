{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS
   (
   -- * Environment
     Env (..)
   -- ** Lenses
   , envAuth
   , envRegion
   , envManager
   , envLogging

   -- ** Creating the environment
   , newEnv

   -- * Synchronous requests
   -- ** Strict
   , send
   -- ** Streaming
   , with
   -- ** Pagination
   , paginate

   -- * Signing URLs
   , presign

   -- * Request primitives
   , open
   , close
   ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Lens                ((^.))
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.Bifunctor
import           Data.ByteString             (ByteString)
import           Data.Monoid
import qualified Data.Text                   as Text
import           Data.Time
import           Network.AWS.Auth
import           Network.AWS.Data
import           Network.AWS.Signing.Common
import           Network.AWS.Types
import           Network.HTTP.Client

-- FIXME: How to constraint send/stream to the correct data types without
-- introducing extraneous/meaningless type classes

data Env = Env
    { _envRegion  :: Region
    , _envLogging :: Logging
    , _envManager :: Manager
    , _envAuth    :: Auth
    }

makeLenses ''Env

newEnv :: MonadBaseControl IO m => Region -> Credentials -> ExceptT Error m Env
newEnv r c = Env r None
    <$> liftBase (newManager defaultManagerSettings)
    <*> getAuth

send :: (MonadBaseControl IO m, AWSRequest a)
     => Env
     -> a
     -> m (Either (Er (Sv a)) (Rs a))
send e rq = with e rq (const . return)

with :: (MonadBaseControl IO m, AWSRequest a)
     => Env
     -> a
     -> (Rs a -> m ByteString -> m b)
     -> m (Either (Er (Sv a)) b)
with e rq f = bracket (open e rq) close go
  where
    go rs = (`catch` er) $ do
        x <- response rq (Right rs)
        either (return . Left)
               (\y -> Right `liftM` f y (responseBody rs))
               x

    er ex = return . Left $ clientError (ex :: HttpException)

paginate :: (MonadBaseControl IO m, AWSPager a)
         => Env
         -> a
         -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginate e rq = fmap (second (next rq) . join (,)) `liftM` send e rq

open :: (MonadBase IO m, AWSRequest a)
     => Env
     -> a
     -> m (ClientResponse m)
open Env{..} (request -> rq) = liftBase $ do
    t  <- getCurrentTime
    sg <- sign _envAuth _envRegion rq t
    debug _envLogging $
        "[Signed Request]\n" <> toText sg
    rs <- responseOpen (sg ^. sgRequest) _envManager
    debug _envLogging $
        "[Raw Response]\n" <> Text.pack (show $ rs { responseBody = () })
    return $ rs { responseBody = liftBase (responseBody rs)  }

close :: MonadBase IO m => ClientResponse m -> m ()
close = liftBase . responseClose

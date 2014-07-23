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

   -- * Synchronous requests
   -- ** Strict
   , send
   -- ** Streaming
   , with
   -- ** Pagination
   , paginate
   -- ** Primitives
   , open
   , close

   -- * Signing URLs
   , presign
   ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception.Lifted
import           Control.Lens                ((^.))
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.Monoid
import qualified Data.Text                   as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.Common
import           Network.AWS.Types
import           Network.HTTP.Client

-- FIXME: How to constraint send/stream to the correct data types without
-- introducing extraneous/meaningless type classes

data Env = Env
    { _envAuth    :: Auth
    , _envRegion  :: Region
    , _envManager :: Manager
    , _envLogging :: Logging
    }

makeLenses ''Env

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
with e rq f = bracket (open e rq) close $ \rs -> do
    debug (_envLogging e) $
        "[Raw Response]\n" <> Text.pack (show $ rs { responseBody = () })
    x <- response rq rs
    either (return . Left)
           (\y -> Right `liftM` f y (liftBase $ responseBody rs))
           x

paginate :: (MonadBaseControl IO m, AWSPager a)
         => Env
         -> a
         -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginate e rq = fmap (second (next rq) . join (,)) `liftM` send e rq

open :: (MonadBase IO m, AWSRequest a)
     => Env
     -> a
     -> m ClientResponse
open Env{..} (request -> rq) = liftBase $ do
    t <- getCurrentTime
    s <- sign _envAuth _envRegion rq t
    debug _envLogging $
        "[Signed Request]\n" <> toText s
    responseOpen (s ^. sgRequest) _envManager

close :: MonadBase IO m => ClientResponse -> m ()
close = liftBase . responseClose

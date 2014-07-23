{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
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
   -- * Sending Requests
   -- ** Synchronous
     send
   -- ** Pagination
   , paginate
   -- ** Streaming
   , with
   -- ** Primitives
   , open
   , close
   -- ** Signed URLs
   , presign
   ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Lens                 ((^.))
import           Control.Monad
import qualified Control.Monad.Catch          as Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Monoid
import qualified Data.Text                    as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Types
import           Network.HTTP.Client

-- FIXME: How to constraint send/stream to the correct data types without
-- introducing extraneous/meaningless type classes

send :: AWSRequest a
     => Auth
     -> Region
     -> Manager
     -> Logging
     -> a
     -> IO (Either (Er (Sv a)) (Rs a))
send a r m l rq = with a r m l rq (const . return)

paginate :: AWSPager a
         => Auth
         -> Region
         -> Manager
         -> Logging
         -> a
         -> IO (Either (Er (Sv a)) (Rs a, Maybe a))
paginate a r m l rq = fmap (second (next rq) . join (,)) `liftM` send a r m l rq

with :: AWSRequest a
     => Auth
     -> Region
     -> Manager
     -> Logging
     -> a
     -> (Rs a -> BodyReader -> IO b)
     -> IO (Either (Er (Sv a)) b)
with a r m l rq f = Catch.bracket (open a r m l rq) close $ \rs -> do
    debug l ("[Raw Response]\n" <> Text.pack (show $ rs { responseBody = () }))
    x <- response rq rs
    either (return . Left)
           (\y -> Right `liftM` f y (responseBody rs))
           x

open :: AWSRequest a
     => Auth
     -> Region
     -> Manager
     -> Logging
     -> a
     -> IO ClientResponse
open a r m l (request -> rq) = do
    t <- getCurrentTime
    s <- sign a r rq t
    debug l ("[Signed Request]\n" <> toText s)
    responseOpen (s ^. sgRequest) m

close :: MonadIO m => ClientResponse -> m ()
close = liftIO . responseClose


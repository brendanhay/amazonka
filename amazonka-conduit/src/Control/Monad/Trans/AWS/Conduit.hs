{-# LANGUAGE FlexibleContexts #-}

-- Module      : Control.Monad.Trans.AWS.Conduit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.Trans.AWS.Conduit where
    -- (
    -- -- * Monad
    --   AWS

    -- -- * Transformer
    -- , AWST
    -- , runAWST

    -- -- * Helpers
    -- , hoistEither
    -- , scoped

    -- -- * Regionalisation
    -- , within

    -- -- * Synchronous requests
    -- -- ** Strict
    -- , send
    -- , sendCatch
    -- -- ** Streaming
    -- , with
    -- , withCatch
    -- -- ** Pagination
    -- , paginate
    -- , paginateCatch

    -- -- * Asynchronous actions
    -- , async
    -- , wait

    -- -- * Signing URLs
    -- , presign
    -- ) where

-- reexport: AWS
-- reexport: AWST
-- reexport: runAWST
-- reexport: hoistEither
-- reexport: within
-- reexport: send
-- reexport: sendCatch
-- reexport: async
-- reexport: wait
-- reexport: presign

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.Trans.AWS     as AWST
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.Conduit
import           Network.AWS                 (Env)
import qualified Network.AWS.Conduit         as AWS
import           Network.AWS.Types

stream :: ( MonadBaseControl IO m
          , MonadReader Env m
          , MonadError Error m
          , AWSRequest a
          )
       => a
       -> m (Rs a, ResumableSource m ByteString)
stream = AWST.hoistEither <=< streamCatch

streamCatch :: (MonadBaseControl IO m, MonadReader Env m, AWSRequest a)
            => a
            -> m (Either (Er (Sv a)) (Rs a, ResumableSource m ByteString))
streamCatch rq = AWST.scoped (\e -> AWS.stream e rq)

paginate :: ( MonadBaseControl IO m
            , MonadReader Env (ResumableSource m)
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> ResumableSource m (Rs a)
paginate rq = paginateCatch rq $=+
    awaitForever (either (throwError . awsError) yield)

paginateCatch :: ( MonadBaseControl IO m
                 , MonadReader Env (ResumableSource m)
                 , AWSPager a
                 )
              => a
              -> ResumableSource m (Either (Er (Sv a)) (Rs a))
paginateCatch rq = AWST.scoped (\e -> AWS.paginate e rq)

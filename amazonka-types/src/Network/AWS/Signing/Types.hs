{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit
import           Data.Default
import           Data.Time
import           Network.AWS.Types
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Types.Header
import           System.Locale

-- type Presignable a = Presign (Sg (Sv a))
-- type Signable    a = Sign (Sg (Sv a))

-- Presign can return the 'raw' Signed HTTP.Request
-- This would mean Signed a v is required to connect the Request/Response
-- Instance for AWSRequest/Signable for Signed a v which doesn't attempt to resign

data Signed v a = Signed
    { _sgMeta    :: Meta v
    , _sgRequest :: Client.Request
    }

sgMeta :: Functor f => LensLike' f (Signed v a) (Meta v)
sgMeta f x = (\y -> x { _sgMeta = y }) <$> f (_sgMeta x)

sgRequest :: Functor f => LensLike' f (Signed v a) Client.Request
sgRequest f x = (\y -> x { _sgRequest = y }) <$> f (_sgRequest x)

data family Meta v :: *

class Signer v where
    sign :: Service s v
         -> Request s a
         -> AuthState
         -> Region
         -> TimeLocale
         -> UTCTime
         -> Signed v a

class Presigner v where
    presign :: Service s v
            -> Request s a
            -> AuthState
            -> Region
            -> TimeLocale
            -> UTCTime
            -> Int
            -> Signed v a

-- class Presigner v where
--     data PMeta v :: *

--     expires :: Service s v
--             -> Request s a
--             -> AuthState
--             -> Region
--             -> TimeLocale
--             -> UTCTime
--             -> Int
--             -> Signed v a (PMeta v)

-- class SigningAlgorithm a => Presign a where
--     expires :: Service b a
--             -> Request b
--             -> AuthState
--             -> Region
--             -> TimeLocale
--             -> UTCTime
--             -> Int
--             -> Ctx a

-- sign :: (AWSRequest a, Signable a)
--      => a
--      -> AuthState
--      -> Region
--      -> UTCTime
--      -> Context (Sg (Sv a))
-- sign rq a r = context service (request rq) a r defaultTimeLocale

conv :: Client.Request
conv = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }

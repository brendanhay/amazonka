{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}

-- |
-- Module      : Network.AWS.Sign.V4
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V4
    ( V4 (..)
    , v4
    ) where

import qualified Data.CaseInsensitive        as CI
import           Data.Monoid
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Query
import           Network.AWS.Data.Time
import           Network.AWS.Lens            ((%~), (<>~))
import           Network.AWS.Request
import           Network.AWS.Sign.V4.Base
import           Network.AWS.Sign.V4.Chunked
import           Network.AWS.Types

default (ByteString)

v4 :: Signer
v4 = Signer sign presign

presign :: Seconds -> Algorithm a
presign ex rq a r ts = signRequest meta mempty auth
  where
    auth = requestQuery <>~ ("&X-Amz-Signature=" <> toBS (metaSignature meta))

    meta = signMetadata a r ts presigner digest (prepare rq)

    presigner c shs =
          pair (CI.original hAMZAlgorithm)     algorithm
        . pair (CI.original hAMZCredential)    (toBS c)
        . pair (CI.original hAMZDate)          (Time ts :: AWSTime)
        . pair (CI.original hAMZExpires)       ex
        . pair (CI.original hAMZSignedHeaders) (toBS shs)
        . pair (CI.original hAMZToken)         (toBS <$> _authSessionToken a)

    digest = Tag "UNSIGNED-PAYLOAD"

    prepare = rqHeaders %~ ( hdr hHost (_endpointHost end) )
    end     = _svcEndpoint (_rqService rq) r

sign :: Algorithm a
sign rq a r ts =
    case _rqBody rq of
        Chunked x -> chunked x rq a r ts
        Hashed  x -> hashed  x rq a r ts

hashed :: HashedBody -> Algorithm a
hashed x rq a r ts =
    let (meta, auth) = base (Tag (sha256Base16 x)) rq a r ts
     in signRequest meta (toRequestBody (Hashed x)) auth

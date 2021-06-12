{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types.S3ObjectEncryptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.S3ObjectEncryptionType
  ( S3ObjectEncryptionType
      ( ..,
        S3ObjectEncryptionType_NO_ENCRYPTION,
        S3ObjectEncryptionType_SSE_KMS,
        S3ObjectEncryptionType_SSE_S3
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype S3ObjectEncryptionType = S3ObjectEncryptionType'
  { fromS3ObjectEncryptionType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern S3ObjectEncryptionType_NO_ENCRYPTION :: S3ObjectEncryptionType
pattern S3ObjectEncryptionType_NO_ENCRYPTION = S3ObjectEncryptionType' "NO_ENCRYPTION"

pattern S3ObjectEncryptionType_SSE_KMS :: S3ObjectEncryptionType
pattern S3ObjectEncryptionType_SSE_KMS = S3ObjectEncryptionType' "SSE_KMS"

pattern S3ObjectEncryptionType_SSE_S3 :: S3ObjectEncryptionType
pattern S3ObjectEncryptionType_SSE_S3 = S3ObjectEncryptionType' "SSE_S3"

{-# COMPLETE
  S3ObjectEncryptionType_NO_ENCRYPTION,
  S3ObjectEncryptionType_SSE_KMS,
  S3ObjectEncryptionType_SSE_S3,
  S3ObjectEncryptionType'
  #-}

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
-- Module      : Network.AWS.Glue.Types.S3EncryptionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3EncryptionMode
  ( S3EncryptionMode
      ( ..,
        S3EncryptionMode_DISABLED,
        S3EncryptionMode_SSE_KMS,
        S3EncryptionMode_SSE_S3
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype S3EncryptionMode = S3EncryptionMode'
  { fromS3EncryptionMode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern S3EncryptionMode_DISABLED :: S3EncryptionMode
pattern S3EncryptionMode_DISABLED = S3EncryptionMode' "DISABLED"

pattern S3EncryptionMode_SSE_KMS :: S3EncryptionMode
pattern S3EncryptionMode_SSE_KMS = S3EncryptionMode' "SSE-KMS"

pattern S3EncryptionMode_SSE_S3 :: S3EncryptionMode
pattern S3EncryptionMode_SSE_S3 = S3EncryptionMode' "SSE-S3"

{-# COMPLETE
  S3EncryptionMode_DISABLED,
  S3EncryptionMode_SSE_KMS,
  S3EncryptionMode_SSE_S3,
  S3EncryptionMode'
  #-}

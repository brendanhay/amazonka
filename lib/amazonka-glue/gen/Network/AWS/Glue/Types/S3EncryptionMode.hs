{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3EncryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3EncryptionMode
  ( S3EncryptionMode
      ( S3EncryptionMode',
        Disabled,
        SseKMS,
        SseS3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype S3EncryptionMode = S3EncryptionMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Disabled :: S3EncryptionMode
pattern Disabled = S3EncryptionMode' "DISABLED"

pattern SseKMS :: S3EncryptionMode
pattern SseKMS = S3EncryptionMode' "SSE-KMS"

pattern SseS3 :: S3EncryptionMode
pattern SseS3 = S3EncryptionMode' "SSE-S3"

{-# COMPLETE
  Disabled,
  SseKMS,
  SseS3,
  S3EncryptionMode'
  #-}

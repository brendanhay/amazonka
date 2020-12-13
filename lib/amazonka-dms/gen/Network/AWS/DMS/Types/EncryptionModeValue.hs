{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EncryptionModeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncryptionModeValue
  ( EncryptionModeValue
      ( EncryptionModeValue',
        SseS3,
        SseKMS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EncryptionModeValue = EncryptionModeValue' Lude.Text
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

pattern SseS3 :: EncryptionModeValue
pattern SseS3 = EncryptionModeValue' "sse-s3"

pattern SseKMS :: EncryptionModeValue
pattern SseKMS = EncryptionModeValue' "sse-kms"

{-# COMPLETE
  SseS3,
  SseKMS,
  EncryptionModeValue'
  #-}

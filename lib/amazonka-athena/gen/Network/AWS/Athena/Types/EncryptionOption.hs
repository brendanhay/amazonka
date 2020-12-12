{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionOption
  ( EncryptionOption
      ( EncryptionOption',
        CseKMS,
        SseKMS,
        SseS3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EncryptionOption = EncryptionOption' Lude.Text
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

pattern CseKMS :: EncryptionOption
pattern CseKMS = EncryptionOption' "CSE_KMS"

pattern SseKMS :: EncryptionOption
pattern SseKMS = EncryptionOption' "SSE_KMS"

pattern SseS3 :: EncryptionOption
pattern SseS3 = EncryptionOption' "SSE_S3"

{-# COMPLETE
  CseKMS,
  SseKMS,
  SseS3,
  EncryptionOption'
  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.S3SseAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.S3SseAlgorithm
  ( S3SseAlgorithm
      ( S3SseAlgorithm',
        SSAAES256,
        SSAKMS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype S3SseAlgorithm = S3SseAlgorithm' Lude.Text
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

pattern SSAAES256 :: S3SseAlgorithm
pattern SSAAES256 = S3SseAlgorithm' "AES256"

pattern SSAKMS :: S3SseAlgorithm
pattern SSAKMS = S3SseAlgorithm' "KMS"

{-# COMPLETE
  SSAAES256,
  SSAKMS,
  S3SseAlgorithm'
  #-}

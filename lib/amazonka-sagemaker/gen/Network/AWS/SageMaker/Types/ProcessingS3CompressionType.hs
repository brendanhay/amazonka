-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3CompressionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3CompressionType
  ( ProcessingS3CompressionType
      ( ProcessingS3CompressionType',
        PSCTGzip,
        PSCTNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingS3CompressionType = ProcessingS3CompressionType' Lude.Text
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

pattern PSCTGzip :: ProcessingS3CompressionType
pattern PSCTGzip = ProcessingS3CompressionType' "Gzip"

pattern PSCTNone :: ProcessingS3CompressionType
pattern PSCTNone = ProcessingS3CompressionType' "None"

{-# COMPLETE
  PSCTGzip,
  PSCTNone,
  ProcessingS3CompressionType'
  #-}

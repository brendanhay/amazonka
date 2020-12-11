-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3DataType
  ( ProcessingS3DataType
      ( ProcessingS3DataType',
        PSDTManifestFile,
        PSDTS3Prefix
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingS3DataType = ProcessingS3DataType' Lude.Text
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

pattern PSDTManifestFile :: ProcessingS3DataType
pattern PSDTManifestFile = ProcessingS3DataType' "ManifestFile"

pattern PSDTS3Prefix :: ProcessingS3DataType
pattern PSDTS3Prefix = ProcessingS3DataType' "S3Prefix"

{-# COMPLETE
  PSDTManifestFile,
  PSDTS3Prefix,
  ProcessingS3DataType'
  #-}

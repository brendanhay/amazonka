{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.S3DataType
  ( S3DataType
    ( S3DataType'
    , S3DataTypeManifestFile
    , S3DataTypeS3Prefix
    , S3DataTypeAugmentedManifestFile
    , fromS3DataType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype S3DataType = S3DataType'{fromS3DataType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern S3DataTypeManifestFile :: S3DataType
pattern S3DataTypeManifestFile = S3DataType' "ManifestFile"

pattern S3DataTypeS3Prefix :: S3DataType
pattern S3DataTypeS3Prefix = S3DataType' "S3Prefix"

pattern S3DataTypeAugmentedManifestFile :: S3DataType
pattern S3DataTypeAugmentedManifestFile = S3DataType' "AugmentedManifestFile"

{-# COMPLETE 
  S3DataTypeManifestFile,

  S3DataTypeS3Prefix,

  S3DataTypeAugmentedManifestFile,
  S3DataType'
  #-}

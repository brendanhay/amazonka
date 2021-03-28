{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLS3DataType
  ( AutoMLS3DataType
    ( AutoMLS3DataType'
    , AutoMLS3DataTypeManifestFile
    , AutoMLS3DataTypeS3Prefix
    , fromAutoMLS3DataType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AutoMLS3DataType = AutoMLS3DataType'{fromAutoMLS3DataType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern AutoMLS3DataTypeManifestFile :: AutoMLS3DataType
pattern AutoMLS3DataTypeManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AutoMLS3DataTypeS3Prefix :: AutoMLS3DataType
pattern AutoMLS3DataTypeS3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE 
  AutoMLS3DataTypeManifestFile,

  AutoMLS3DataTypeS3Prefix,
  AutoMLS3DataType'
  #-}

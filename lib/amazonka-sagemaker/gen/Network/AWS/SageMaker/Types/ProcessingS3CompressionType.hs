{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3CompressionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3CompressionType
  ( ProcessingS3CompressionType
    ( ProcessingS3CompressionType'
    , ProcessingS3CompressionTypeNone
    , ProcessingS3CompressionTypeGzip
    , fromProcessingS3CompressionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProcessingS3CompressionType = ProcessingS3CompressionType'{fromProcessingS3CompressionType
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern ProcessingS3CompressionTypeNone :: ProcessingS3CompressionType
pattern ProcessingS3CompressionTypeNone = ProcessingS3CompressionType' "None"

pattern ProcessingS3CompressionTypeGzip :: ProcessingS3CompressionType
pattern ProcessingS3CompressionTypeGzip = ProcessingS3CompressionType' "Gzip"

{-# COMPLETE 
  ProcessingS3CompressionTypeNone,

  ProcessingS3CompressionTypeGzip,
  ProcessingS3CompressionType'
  #-}

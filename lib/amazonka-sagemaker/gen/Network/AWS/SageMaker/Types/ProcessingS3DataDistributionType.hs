{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
  ( ProcessingS3DataDistributionType
    ( ProcessingS3DataDistributionType'
    , ProcessingS3DataDistributionTypeFullyReplicated
    , ProcessingS3DataDistributionTypeShardedByS3Key
    , fromProcessingS3DataDistributionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProcessingS3DataDistributionType = ProcessingS3DataDistributionType'{fromProcessingS3DataDistributionType
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern ProcessingS3DataDistributionTypeFullyReplicated :: ProcessingS3DataDistributionType
pattern ProcessingS3DataDistributionTypeFullyReplicated = ProcessingS3DataDistributionType' "FullyReplicated"

pattern ProcessingS3DataDistributionTypeShardedByS3Key :: ProcessingS3DataDistributionType
pattern ProcessingS3DataDistributionTypeShardedByS3Key = ProcessingS3DataDistributionType' "ShardedByS3Key"

{-# COMPLETE 
  ProcessingS3DataDistributionTypeFullyReplicated,

  ProcessingS3DataDistributionTypeShardedByS3Key,
  ProcessingS3DataDistributionType'
  #-}

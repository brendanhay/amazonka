{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataDistribution
  ( S3DataDistribution
      ( S3DataDistribution',
        S3DataDistributionFullyReplicated,
        S3DataDistributionShardedByS3Key,
        fromS3DataDistribution
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype S3DataDistribution = S3DataDistribution'
  { fromS3DataDistribution ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern S3DataDistributionFullyReplicated :: S3DataDistribution
pattern S3DataDistributionFullyReplicated = S3DataDistribution' "FullyReplicated"

pattern S3DataDistributionShardedByS3Key :: S3DataDistribution
pattern S3DataDistributionShardedByS3Key = S3DataDistribution' "ShardedByS3Key"

{-# COMPLETE
  S3DataDistributionFullyReplicated,
  S3DataDistributionShardedByS3Key,
  S3DataDistribution'
  #-}

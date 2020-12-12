{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
  ( ProcessingS3DataDistributionType
      ( ProcessingS3DataDistributionType',
        PSDDTFullyReplicated,
        PSDDTShardedByS3Key
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingS3DataDistributionType = ProcessingS3DataDistributionType' Lude.Text
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

pattern PSDDTFullyReplicated :: ProcessingS3DataDistributionType
pattern PSDDTFullyReplicated = ProcessingS3DataDistributionType' "FullyReplicated"

pattern PSDDTShardedByS3Key :: ProcessingS3DataDistributionType
pattern PSDDTShardedByS3Key = ProcessingS3DataDistributionType' "ShardedByS3Key"

{-# COMPLETE
  PSDDTFullyReplicated,
  PSDDTShardedByS3Key,
  ProcessingS3DataDistributionType'
  #-}

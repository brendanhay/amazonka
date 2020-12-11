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
        FullyReplicated,
        ShardedByS3Key
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype S3DataDistribution = S3DataDistribution' Lude.Text
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

pattern FullyReplicated :: S3DataDistribution
pattern FullyReplicated = S3DataDistribution' "FullyReplicated"

pattern ShardedByS3Key :: S3DataDistribution
pattern ShardedByS3Key = S3DataDistribution' "ShardedByS3Key"

{-# COMPLETE
  FullyReplicated,
  ShardedByS3Key,
  S3DataDistribution'
  #-}

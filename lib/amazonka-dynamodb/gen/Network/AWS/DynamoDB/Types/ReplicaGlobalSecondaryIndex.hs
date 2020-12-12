{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
  ( ReplicaGlobalSecondaryIndex (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndex,

    -- * Lenses
    rgsiProvisionedThroughputOverride,
    rgsiIndexName,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndex' smart constructor.
data ReplicaGlobalSecondaryIndex = ReplicaGlobalSecondaryIndex'
  { provisionedThroughputOverride ::
      Lude.Maybe
        ProvisionedThroughputOverride,
    indexName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaGlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index.
-- * 'provisionedThroughputOverride' - Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
mkReplicaGlobalSecondaryIndex ::
  -- | 'indexName'
  Lude.Text ->
  ReplicaGlobalSecondaryIndex
mkReplicaGlobalSecondaryIndex pIndexName_ =
  ReplicaGlobalSecondaryIndex'
    { provisionedThroughputOverride =
        Lude.Nothing,
      indexName = pIndexName_
    }

-- | Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiProvisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndex (Lude.Maybe ProvisionedThroughputOverride)
rgsiProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: ReplicaGlobalSecondaryIndex -> Lude.Maybe ProvisionedThroughputOverride) (\s a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndex)
{-# DEPRECATED rgsiProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndex Lude.Text
rgsiIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndex -> Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndex)
{-# DEPRECATED rgsiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON ReplicaGlobalSecondaryIndex where
  toJSON ReplicaGlobalSecondaryIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedThroughputOverride" Lude..=)
              Lude.<$> provisionedThroughputOverride,
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )

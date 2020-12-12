{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
  ( ReplicaGlobalSecondaryIndexDescription (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexDescription,

    -- * Lenses
    rgsidProvisionedThroughputOverride,
    rgsidIndexName,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexDescription' smart constructor.
data ReplicaGlobalSecondaryIndexDescription = ReplicaGlobalSecondaryIndexDescription'
  { provisionedThroughputOverride ::
      Lude.Maybe
        ProvisionedThroughputOverride,
    indexName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaGlobalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index.
-- * 'provisionedThroughputOverride' - If not described, uses the source table GSI's read capacity settings.
mkReplicaGlobalSecondaryIndexDescription ::
  ReplicaGlobalSecondaryIndexDescription
mkReplicaGlobalSecondaryIndexDescription =
  ReplicaGlobalSecondaryIndexDescription'
    { provisionedThroughputOverride =
        Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | If not described, uses the source table GSI's read capacity settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsidProvisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Lude.Maybe ProvisionedThroughputOverride)
rgsidProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: ReplicaGlobalSecondaryIndexDescription -> Lude.Maybe ProvisionedThroughputOverride) (\s a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndexDescription)
{-# DEPRECATED rgsidProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsidIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Lude.Maybe Lude.Text)
rgsidIndexName = Lens.lens (indexName :: ReplicaGlobalSecondaryIndexDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexDescription)
{-# DEPRECATED rgsidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON ReplicaGlobalSecondaryIndexDescription where
  parseJSON =
    Lude.withObject
      "ReplicaGlobalSecondaryIndexDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexDescription'
            Lude.<$> (x Lude..:? "ProvisionedThroughputOverride")
            Lude.<*> (x Lude..:? "IndexName")
      )

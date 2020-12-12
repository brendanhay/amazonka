{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
  ( GlobalTableGlobalSecondaryIndexSettingsUpdate (..),

    -- * Smart constructor
    mkGlobalTableGlobalSecondaryIndexSettingsUpdate,

    -- * Lenses
    gtgsisuProvisionedWriteCapacityUnits,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkGlobalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { provisionedWriteCapacityUnits ::
      Lude.Maybe
        Lude.Natural,
    provisionedWriteCapacityAutoScalingSettingsUpdate ::
      Lude.Maybe
        AutoScalingSettingsUpdate,
    indexName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GlobalTableGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
-- * 'provisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global secondary index's write capacity units.
-- * 'provisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
mkGlobalTableGlobalSecondaryIndexSettingsUpdate ::
  -- | 'indexName'
  Lude.Text ->
  GlobalTableGlobalSecondaryIndexSettingsUpdate
mkGlobalTableGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  GlobalTableGlobalSecondaryIndexSettingsUpdate'
    { provisionedWriteCapacityUnits =
        Lude.Nothing,
      provisionedWriteCapacityAutoScalingSettingsUpdate =
        Lude.Nothing,
      indexName = pIndexName_
    }

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- /Note:/ Consider using 'provisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuProvisionedWriteCapacityUnits :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Lude.Maybe Lude.Natural)
gtgsisuProvisionedWriteCapacityUnits = Lens.lens (provisionedWriteCapacityUnits :: GlobalTableGlobalSecondaryIndexSettingsUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedWriteCapacityUnits = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED gtgsisuProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'provisionedWriteCapacityUnits' instead." #-}

-- | Auto scaling settings for managing a global secondary index's write capacity units.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Lude.Maybe AutoScalingSettingsUpdate)
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (provisionedWriteCapacityAutoScalingSettingsUpdate :: GlobalTableGlobalSecondaryIndexSettingsUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {provisionedWriteCapacityAutoScalingSettingsUpdate = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuIndexName :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Lude.Text
gtgsisuIndexName = Lens.lens (indexName :: GlobalTableGlobalSecondaryIndexSettingsUpdate -> Lude.Text) (\s a -> s {indexName = a} :: GlobalTableGlobalSecondaryIndexSettingsUpdate)
{-# DEPRECATED gtgsisuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON GlobalTableGlobalSecondaryIndexSettingsUpdate where
  toJSON GlobalTableGlobalSecondaryIndexSettingsUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedWriteCapacityUnits" Lude..=)
              Lude.<$> provisionedWriteCapacityUnits,
            ("ProvisionedWriteCapacityAutoScalingSettingsUpdate" Lude..=)
              Lude.<$> provisionedWriteCapacityAutoScalingSettingsUpdate,
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )

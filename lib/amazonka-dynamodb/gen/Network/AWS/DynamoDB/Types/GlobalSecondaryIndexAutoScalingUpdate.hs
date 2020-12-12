{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
  ( GlobalSecondaryIndexAutoScalingUpdate (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexAutoScalingUpdate,

    -- * Lenses
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,
    gsiasuIndexName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data GlobalSecondaryIndexAutoScalingUpdate = GlobalSecondaryIndexAutoScalingUpdate'
  { provisionedWriteCapacityAutoScalingUpdate ::
      Lude.Maybe
        AutoScalingSettingsUpdate,
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

-- | Creates a value of 'GlobalSecondaryIndexAutoScalingUpdate' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index.
-- * 'provisionedWriteCapacityAutoScalingUpdate' - Undocumented field.
mkGlobalSecondaryIndexAutoScalingUpdate ::
  GlobalSecondaryIndexAutoScalingUpdate
mkGlobalSecondaryIndexAutoScalingUpdate =
  GlobalSecondaryIndexAutoScalingUpdate'
    { provisionedWriteCapacityAutoScalingUpdate =
        Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiasuProvisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Lude.Maybe AutoScalingSettingsUpdate)
gsiasuProvisionedWriteCapacityAutoScalingUpdate = Lens.lens (provisionedWriteCapacityAutoScalingUpdate :: GlobalSecondaryIndexAutoScalingUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: GlobalSecondaryIndexAutoScalingUpdate)
{-# DEPRECATED gsiasuProvisionedWriteCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingUpdate' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiasuIndexName :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Lude.Maybe Lude.Text)
gsiasuIndexName = Lens.lens (indexName :: GlobalSecondaryIndexAutoScalingUpdate -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GlobalSecondaryIndexAutoScalingUpdate)
{-# DEPRECATED gsiasuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON GlobalSecondaryIndexAutoScalingUpdate where
  toJSON GlobalSecondaryIndexAutoScalingUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedWriteCapacityAutoScalingUpdate" Lude..=)
              Lude.<$> provisionedWriteCapacityAutoScalingUpdate,
            ("IndexName" Lude..=) Lude.<$> indexName
          ]
      )

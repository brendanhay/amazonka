{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetActionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetActionSummary
  ( DatasetActionSummary (..),

    -- * Smart constructor
    mkDatasetActionSummary,

    -- * Lenses
    dasActionName,
    dasActionType,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DatasetActionName as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the action that automatically creates the dataset's contents.
--
-- /See:/ 'mkDatasetActionSummary' smart constructor.
data DatasetActionSummary = DatasetActionSummary'
  { -- | The name of the action that automatically creates the dataset's contents.
    actionName :: Core.Maybe Types.DatasetActionName,
    -- | The type of action by which the dataset's contents are automatically created.
    actionType :: Core.Maybe Types.DatasetActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetActionSummary' value with any optional fields omitted.
mkDatasetActionSummary ::
  DatasetActionSummary
mkDatasetActionSummary =
  DatasetActionSummary'
    { actionName = Core.Nothing,
      actionType = Core.Nothing
    }

-- | The name of the action that automatically creates the dataset's contents.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasActionName :: Lens.Lens' DatasetActionSummary (Core.Maybe Types.DatasetActionName)
dasActionName = Lens.field @"actionName"
{-# DEPRECATED dasActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The type of action by which the dataset's contents are automatically created.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasActionType :: Lens.Lens' DatasetActionSummary (Core.Maybe Types.DatasetActionType)
dasActionType = Lens.field @"actionType"
{-# DEPRECATED dasActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Core.FromJSON DatasetActionSummary where
  parseJSON =
    Core.withObject "DatasetActionSummary" Core.$
      \x ->
        DatasetActionSummary'
          Core.<$> (x Core..:? "actionName") Core.<*> (x Core..:? "actionType")

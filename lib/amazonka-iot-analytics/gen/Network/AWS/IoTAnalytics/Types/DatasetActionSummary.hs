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

import Network.AWS.IoTAnalytics.Types.DatasetActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the action that automatically creates the dataset's contents.
--
-- /See:/ 'mkDatasetActionSummary' smart constructor.
data DatasetActionSummary = DatasetActionSummary'
  { actionName ::
      Lude.Maybe Lude.Text,
    actionType :: Lude.Maybe DatasetActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetActionSummary' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the action that automatically creates the dataset's contents.
-- * 'actionType' - The type of action by which the dataset's contents are automatically created.
mkDatasetActionSummary ::
  DatasetActionSummary
mkDatasetActionSummary =
  DatasetActionSummary'
    { actionName = Lude.Nothing,
      actionType = Lude.Nothing
    }

-- | The name of the action that automatically creates the dataset's contents.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasActionName :: Lens.Lens' DatasetActionSummary (Lude.Maybe Lude.Text)
dasActionName = Lens.lens (actionName :: DatasetActionSummary -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: DatasetActionSummary)
{-# DEPRECATED dasActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The type of action by which the dataset's contents are automatically created.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasActionType :: Lens.Lens' DatasetActionSummary (Lude.Maybe DatasetActionType)
dasActionType = Lens.lens (actionType :: DatasetActionSummary -> Lude.Maybe DatasetActionType) (\s a -> s {actionType = a} :: DatasetActionSummary)
{-# DEPRECATED dasActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Lude.FromJSON DatasetActionSummary where
  parseJSON =
    Lude.withObject
      "DatasetActionSummary"
      ( \x ->
          DatasetActionSummary'
            Lude.<$> (x Lude..:? "actionName") Lude.<*> (x Lude..:? "actionType")
      )

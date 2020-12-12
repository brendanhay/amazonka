{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetAction
  ( DatasetAction (..),

    -- * Smart constructor
    mkDatasetAction,

    -- * Lenses
    daQueryAction,
    daActionName,
    daContainerAction,
  )
where

import Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
import Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @DatasetAction@ object that specifies how data set contents are automatically created.
--
-- /See:/ 'mkDatasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { queryAction ::
      Lude.Maybe SqlQueryDatasetAction,
    actionName :: Lude.Maybe Lude.Text,
    containerAction :: Lude.Maybe ContainerDatasetAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetAction' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the data set action by which data set contents are automatically created.
-- * 'containerAction' - Information that allows the system to run a containerized application to create the dataset contents. The application must be in a Docker container along with any required support libraries.
-- * 'queryAction' - An @SqlQueryDatasetAction@ object that uses an SQL query to automatically create data set contents.
mkDatasetAction ::
  DatasetAction
mkDatasetAction =
  DatasetAction'
    { queryAction = Lude.Nothing,
      actionName = Lude.Nothing,
      containerAction = Lude.Nothing
    }

-- | An @SqlQueryDatasetAction@ object that uses an SQL query to automatically create data set contents.
--
-- /Note:/ Consider using 'queryAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daQueryAction :: Lens.Lens' DatasetAction (Lude.Maybe SqlQueryDatasetAction)
daQueryAction = Lens.lens (queryAction :: DatasetAction -> Lude.Maybe SqlQueryDatasetAction) (\s a -> s {queryAction = a} :: DatasetAction)
{-# DEPRECATED daQueryAction "Use generic-lens or generic-optics with 'queryAction' instead." #-}

-- | The name of the data set action by which data set contents are automatically created.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActionName :: Lens.Lens' DatasetAction (Lude.Maybe Lude.Text)
daActionName = Lens.lens (actionName :: DatasetAction -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: DatasetAction)
{-# DEPRECATED daActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Information that allows the system to run a containerized application to create the dataset contents. The application must be in a Docker container along with any required support libraries.
--
-- /Note:/ Consider using 'containerAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daContainerAction :: Lens.Lens' DatasetAction (Lude.Maybe ContainerDatasetAction)
daContainerAction = Lens.lens (containerAction :: DatasetAction -> Lude.Maybe ContainerDatasetAction) (\s a -> s {containerAction = a} :: DatasetAction)
{-# DEPRECATED daContainerAction "Use generic-lens or generic-optics with 'containerAction' instead." #-}

instance Lude.FromJSON DatasetAction where
  parseJSON =
    Lude.withObject
      "DatasetAction"
      ( \x ->
          DatasetAction'
            Lude.<$> (x Lude..:? "queryAction")
            Lude.<*> (x Lude..:? "actionName")
            Lude.<*> (x Lude..:? "containerAction")
      )

instance Lude.ToJSON DatasetAction where
  toJSON DatasetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryAction" Lude..=) Lude.<$> queryAction,
            ("actionName" Lude..=) Lude.<$> actionName,
            ("containerAction" Lude..=) Lude.<$> containerAction
          ]
      )

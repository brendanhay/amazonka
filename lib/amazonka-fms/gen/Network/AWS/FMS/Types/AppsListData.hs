{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AppsListData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListData
  ( AppsListData (..),

    -- * Smart constructor
    mkAppsListData,

    -- * Lenses
    aldListName,
    aldAppsList,
    aldCreateTime,
    aldLastUpdateTime,
    aldListId,
    aldListUpdateToken,
    aldPreviousAppsList,
  )
where

import qualified Network.AWS.FMS.Types.App as Types
import qualified Network.AWS.FMS.Types.ListId as Types
import qualified Network.AWS.FMS.Types.PreviousListVersion as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.FMS.Types.UpdateToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An AWS Firewall Manager applications list.
--
-- /See:/ 'mkAppsListData' smart constructor.
data AppsListData = AppsListData'
  { -- | The name of the AWS Firewall Manager applications list.
    listName :: Types.ResourceName,
    -- | An array of applications in the AWS Firewall Manager applications list.
    appsList :: [Types.App],
    -- | The time that the AWS Firewall Manager applications list was created.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time that the AWS Firewall Manager applications list was last updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the AWS Firewall Manager applications list.
    listId :: Core.Maybe Types.ListId,
    -- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
    listUpdateToken :: Core.Maybe Types.UpdateToken,
    -- | A map of previous version numbers to their corresponding @App@ object arrays.
    previousAppsList :: Core.Maybe (Core.HashMap Types.PreviousListVersion [Types.App])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AppsListData' value with any optional fields omitted.
mkAppsListData ::
  -- | 'listName'
  Types.ResourceName ->
  AppsListData
mkAppsListData listName =
  AppsListData'
    { listName,
      appsList = Core.mempty,
      createTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      listId = Core.Nothing,
      listUpdateToken = Core.Nothing,
      previousAppsList = Core.Nothing
    }

-- | The name of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListName :: Lens.Lens' AppsListData Types.ResourceName
aldListName = Lens.field @"listName"
{-# DEPRECATED aldListName "Use generic-lens or generic-optics with 'listName' instead." #-}

-- | An array of applications in the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldAppsList :: Lens.Lens' AppsListData [Types.App]
aldAppsList = Lens.field @"appsList"
{-# DEPRECATED aldAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The time that the AWS Firewall Manager applications list was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldCreateTime :: Lens.Lens' AppsListData (Core.Maybe Core.NominalDiffTime)
aldCreateTime = Lens.field @"createTime"
{-# DEPRECATED aldCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The time that the AWS Firewall Manager applications list was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldLastUpdateTime :: Lens.Lens' AppsListData (Core.Maybe Core.NominalDiffTime)
aldLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED aldLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The ID of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListId :: Lens.Lens' AppsListData (Core.Maybe Types.ListId)
aldListId = Lens.field @"listId"
{-# DEPRECATED aldListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
--
-- /Note:/ Consider using 'listUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListUpdateToken :: Lens.Lens' AppsListData (Core.Maybe Types.UpdateToken)
aldListUpdateToken = Lens.field @"listUpdateToken"
{-# DEPRECATED aldListUpdateToken "Use generic-lens or generic-optics with 'listUpdateToken' instead." #-}

-- | A map of previous version numbers to their corresponding @App@ object arrays.
--
-- /Note:/ Consider using 'previousAppsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldPreviousAppsList :: Lens.Lens' AppsListData (Core.Maybe (Core.HashMap Types.PreviousListVersion [Types.App]))
aldPreviousAppsList = Lens.field @"previousAppsList"
{-# DEPRECATED aldPreviousAppsList "Use generic-lens or generic-optics with 'previousAppsList' instead." #-}

instance Core.FromJSON AppsListData where
  toJSON AppsListData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ListName" Core..= listName),
            Core.Just ("AppsList" Core..= appsList),
            ("CreateTime" Core..=) Core.<$> createTime,
            ("LastUpdateTime" Core..=) Core.<$> lastUpdateTime,
            ("ListId" Core..=) Core.<$> listId,
            ("ListUpdateToken" Core..=) Core.<$> listUpdateToken,
            ("PreviousAppsList" Core..=) Core.<$> previousAppsList
          ]
      )

instance Core.FromJSON AppsListData where
  parseJSON =
    Core.withObject "AppsListData" Core.$
      \x ->
        AppsListData'
          Core.<$> (x Core..: "ListName")
          Core.<*> (x Core..:? "AppsList" Core..!= Core.mempty)
          Core.<*> (x Core..:? "CreateTime")
          Core.<*> (x Core..:? "LastUpdateTime")
          Core.<*> (x Core..:? "ListId")
          Core.<*> (x Core..:? "ListUpdateToken")
          Core.<*> (x Core..:? "PreviousAppsList")

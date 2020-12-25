{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AppsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListDataSummary
  ( AppsListDataSummary (..),

    -- * Smart constructor
    mkAppsListDataSummary,

    -- * Lenses
    aldsAppsList,
    aldsListArn,
    aldsListId,
    aldsListName,
  )
where

import qualified Network.AWS.FMS.Types.App as Types
import qualified Network.AWS.FMS.Types.ListArn as Types
import qualified Network.AWS.FMS.Types.ListId as Types
import qualified Network.AWS.FMS.Types.ListName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the AWS Firewall Manager applications list.
--
-- /See:/ 'mkAppsListDataSummary' smart constructor.
data AppsListDataSummary = AppsListDataSummary'
  { -- | An array of @App@ objects in the AWS Firewall Manager applications list.
    appsList :: Core.Maybe [Types.App],
    -- | The Amazon Resource Name (ARN) of the applications list.
    listArn :: Core.Maybe Types.ListArn,
    -- | The ID of the applications list.
    listId :: Core.Maybe Types.ListId,
    -- | The name of the applications list.
    listName :: Core.Maybe Types.ListName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppsListDataSummary' value with any optional fields omitted.
mkAppsListDataSummary ::
  AppsListDataSummary
mkAppsListDataSummary =
  AppsListDataSummary'
    { appsList = Core.Nothing,
      listArn = Core.Nothing,
      listId = Core.Nothing,
      listName = Core.Nothing
    }

-- | An array of @App@ objects in the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsAppsList :: Lens.Lens' AppsListDataSummary (Core.Maybe [Types.App])
aldsAppsList = Lens.field @"appsList"
{-# DEPRECATED aldsAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'listArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListArn :: Lens.Lens' AppsListDataSummary (Core.Maybe Types.ListArn)
aldsListArn = Lens.field @"listArn"
{-# DEPRECATED aldsListArn "Use generic-lens or generic-optics with 'listArn' instead." #-}

-- | The ID of the applications list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListId :: Lens.Lens' AppsListDataSummary (Core.Maybe Types.ListId)
aldsListId = Lens.field @"listId"
{-# DEPRECATED aldsListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | The name of the applications list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListName :: Lens.Lens' AppsListDataSummary (Core.Maybe Types.ListName)
aldsListName = Lens.field @"listName"
{-# DEPRECATED aldsListName "Use generic-lens or generic-optics with 'listName' instead." #-}

instance Core.FromJSON AppsListDataSummary where
  parseJSON =
    Core.withObject "AppsListDataSummary" Core.$
      \x ->
        AppsListDataSummary'
          Core.<$> (x Core..:? "AppsList")
          Core.<*> (x Core..:? "ListArn")
          Core.<*> (x Core..:? "ListId")
          Core.<*> (x Core..:? "ListName")

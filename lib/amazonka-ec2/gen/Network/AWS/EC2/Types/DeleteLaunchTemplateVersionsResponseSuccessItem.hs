{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
  ( DeleteLaunchTemplateVersionsResponseSuccessItem (..),

    -- * Smart constructor
    mkDeleteLaunchTemplateVersionsResponseSuccessItem,

    -- * Lenses
    dltvrsiLaunchTemplateId,
    dltvrsiLaunchTemplateName,
    dltvrsiVersionNumber,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template version that was successfully deleted.
--
-- /See:/ 'mkDeleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Types.String,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Types.String,
    -- | The version number of the launch template.
    versionNumber :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplateVersionsResponseSuccessItem' value with any optional fields omitted.
mkDeleteLaunchTemplateVersionsResponseSuccessItem ::
  DeleteLaunchTemplateVersionsResponseSuccessItem
mkDeleteLaunchTemplateVersionsResponseSuccessItem =
  DeleteLaunchTemplateVersionsResponseSuccessItem'
    { launchTemplateId =
        Core.Nothing,
      launchTemplateName = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Types.String)
dltvrsiLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED dltvrsiLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Types.String)
dltvrsiLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED dltvrsiLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The version number of the launch template.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiVersionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Core.Integer)
dltvrsiVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED dltvrsiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance
  Core.FromXML
    DeleteLaunchTemplateVersionsResponseSuccessItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseSuccessItem'
      Core.<$> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> (x Core..@? "versionNumber")

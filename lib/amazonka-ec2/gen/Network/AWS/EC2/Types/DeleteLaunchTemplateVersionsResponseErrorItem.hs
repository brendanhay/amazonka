{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
  ( DeleteLaunchTemplateVersionsResponseErrorItem (..),

    -- * Smart constructor
    mkDeleteLaunchTemplateVersionsResponseErrorItem,

    -- * Lenses
    dltvreiLaunchTemplateId,
    dltvreiLaunchTemplateName,
    dltvreiResponseError,
    dltvreiVersionNumber,
  )
where

import qualified Network.AWS.EC2.Types.ResponseError as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template version that could not be deleted.
--
-- /See:/ 'mkDeleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Types.String,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Types.String,
    -- | Information about the error.
    responseError :: Core.Maybe Types.ResponseError,
    -- | The version number of the launch template.
    versionNumber :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplateVersionsResponseErrorItem' value with any optional fields omitted.
mkDeleteLaunchTemplateVersionsResponseErrorItem ::
  DeleteLaunchTemplateVersionsResponseErrorItem
mkDeleteLaunchTemplateVersionsResponseErrorItem =
  DeleteLaunchTemplateVersionsResponseErrorItem'
    { launchTemplateId =
        Core.Nothing,
      launchTemplateName = Core.Nothing,
      responseError = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Types.String)
dltvreiLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED dltvreiLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Types.String)
dltvreiLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED dltvreiLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | Information about the error.
--
-- /Note:/ Consider using 'responseError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiResponseError :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Types.ResponseError)
dltvreiResponseError = Lens.field @"responseError"
{-# DEPRECATED dltvreiResponseError "Use generic-lens or generic-optics with 'responseError' instead." #-}

-- | The version number of the launch template.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiVersionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Core.Maybe Core.Integer)
dltvreiVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED dltvreiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromXML DeleteLaunchTemplateVersionsResponseErrorItem where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseErrorItem'
      Core.<$> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> (x Core..@? "responseError")
      Core.<*> (x Core..@? "versionNumber")

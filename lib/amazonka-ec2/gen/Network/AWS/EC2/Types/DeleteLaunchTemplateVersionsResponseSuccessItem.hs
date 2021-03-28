{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
  ( DeleteLaunchTemplateVersionsResponseSuccessItem (..)
  -- * Smart constructor
  , mkDeleteLaunchTemplateVersionsResponseSuccessItem
  -- * Lenses
  , dltvrsiLaunchTemplateId
  , dltvrsiLaunchTemplateName
  , dltvrsiVersionNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template version that was successfully deleted.
--
-- /See:/ 'mkDeleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { launchTemplateId :: Core.Maybe Core.Text
    -- ^ The ID of the launch template.
  , launchTemplateName :: Core.Maybe Core.Text
    -- ^ The name of the launch template.
  , versionNumber :: Core.Maybe Core.Integer
    -- ^ The version number of the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplateVersionsResponseSuccessItem' value with any optional fields omitted.
mkDeleteLaunchTemplateVersionsResponseSuccessItem
    :: DeleteLaunchTemplateVersionsResponseSuccessItem
mkDeleteLaunchTemplateVersionsResponseSuccessItem
  = DeleteLaunchTemplateVersionsResponseSuccessItem'{launchTemplateId
                                                       = Core.Nothing,
                                                     launchTemplateName = Core.Nothing,
                                                     versionNumber = Core.Nothing}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Core.Text)
dltvrsiLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE dltvrsiLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Core.Text)
dltvrsiLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE dltvrsiLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The version number of the launch template.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiVersionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Core.Maybe Core.Integer)
dltvrsiVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE dltvrsiVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

instance Core.FromXML
           DeleteLaunchTemplateVersionsResponseSuccessItem
         where
        parseXML x
          = DeleteLaunchTemplateVersionsResponseSuccessItem' Core.<$>
              (x Core..@? "launchTemplateId") Core.<*>
                x Core..@? "launchTemplateName"
                Core.<*> x Core..@? "versionNumber"

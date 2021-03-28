{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateVersion
  ( LaunchTemplateVersion (..)
  -- * Smart constructor
  , mkLaunchTemplateVersion
  -- * Lenses
  , ltvCreateTime
  , ltvCreatedBy
  , ltvDefaultVersion
  , ltvLaunchTemplateData
  , ltvLaunchTemplateId
  , ltvLaunchTemplateName
  , ltvVersionDescription
  , ltvVersionNumber
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.EC2.Types.ResponseLaunchTemplateData as Types
import qualified Network.AWS.EC2.Types.VersionDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template version.
--
-- /See:/ 'mkLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { createTime :: Core.Maybe Core.UTCTime
    -- ^ The time the version was created.
  , createdBy :: Core.Maybe Core.Text
    -- ^ The principal that created the version.
  , defaultVersion :: Core.Maybe Core.Bool
    -- ^ Indicates whether the version is the default version.
  , launchTemplateData :: Core.Maybe Types.ResponseLaunchTemplateData
    -- ^ Information about the launch template.
  , launchTemplateId :: Core.Maybe Core.Text
    -- ^ The ID of the launch template.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template.
  , versionDescription :: Core.Maybe Types.VersionDescription
    -- ^ The description for the version.
  , versionNumber :: Core.Maybe Core.Integer
    -- ^ The version number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LaunchTemplateVersion' value with any optional fields omitted.
mkLaunchTemplateVersion
    :: LaunchTemplateVersion
mkLaunchTemplateVersion
  = LaunchTemplateVersion'{createTime = Core.Nothing,
                           createdBy = Core.Nothing, defaultVersion = Core.Nothing,
                           launchTemplateData = Core.Nothing, launchTemplateId = Core.Nothing,
                           launchTemplateName = Core.Nothing,
                           versionDescription = Core.Nothing, versionNumber = Core.Nothing}

-- | The time the version was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreateTime :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.UTCTime)
ltvCreateTime = Lens.field @"createTime"
{-# INLINEABLE ltvCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The principal that created the version.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreatedBy :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
ltvCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE ltvCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | Indicates whether the version is the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvDefaultVersion :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Bool)
ltvDefaultVersion = Lens.field @"defaultVersion"
{-# INLINEABLE ltvDefaultVersion #-}
{-# DEPRECATED defaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead"  #-}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateData :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.ResponseLaunchTemplateData)
ltvLaunchTemplateData = Lens.field @"launchTemplateData"
{-# INLINEABLE ltvLaunchTemplateData #-}
{-# DEPRECATED launchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead"  #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateId :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
ltvLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE ltvLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateName :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.LaunchTemplateName)
ltvLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE ltvLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The description for the version.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionDescription :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.VersionDescription)
ltvVersionDescription = Lens.field @"versionDescription"
{-# INLINEABLE ltvVersionDescription #-}
{-# DEPRECATED versionDescription "Use generic-lens or generic-optics with 'versionDescription' instead"  #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionNumber :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Integer)
ltvVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE ltvVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

instance Core.FromXML LaunchTemplateVersion where
        parseXML x
          = LaunchTemplateVersion' Core.<$>
              (x Core..@? "createTime") Core.<*> x Core..@? "createdBy" Core.<*>
                x Core..@? "defaultVersion"
                Core.<*> x Core..@? "launchTemplateData"
                Core.<*> x Core..@? "launchTemplateId"
                Core.<*> x Core..@? "launchTemplateName"
                Core.<*> x Core..@? "versionDescription"
                Core.<*> x Core..@? "versionNumber"

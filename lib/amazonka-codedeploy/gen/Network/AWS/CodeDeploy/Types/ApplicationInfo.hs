{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ApplicationInfo
  ( ApplicationInfo (..),

    -- * Smart constructor
    mkApplicationInfo,

    -- * Lenses
    aiApplicationId,
    aiApplicationName,
    aiComputePlatform,
    aiCreateTime,
    aiGitHubAccountName,
    aiLinkedToGitHub,
  )
where

import qualified Network.AWS.CodeDeploy.Types.ApplicationId as Types
import qualified Network.AWS.CodeDeploy.Types.ApplicationName as Types
import qualified Network.AWS.CodeDeploy.Types.ComputePlatform as Types
import qualified Network.AWS.CodeDeploy.Types.GitHubAccountName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an application.
--
-- /See:/ 'mkApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { -- | The application ID.
    applicationId :: Core.Maybe Types.ApplicationId,
    -- | The application name.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
    computePlatform :: Core.Maybe Types.ComputePlatform,
    -- | The time at which the application was created.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name for a connection to a GitHub account.
    gitHubAccountName :: Core.Maybe Types.GitHubAccountName,
    -- | True if the user has authenticated with GitHub for the specified application. Otherwise, false.
    linkedToGitHub :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplicationInfo' value with any optional fields omitted.
mkApplicationInfo ::
  ApplicationInfo
mkApplicationInfo =
  ApplicationInfo'
    { applicationId = Core.Nothing,
      applicationName = Core.Nothing,
      computePlatform = Core.Nothing,
      createTime = Core.Nothing,
      gitHubAccountName = Core.Nothing,
      linkedToGitHub = Core.Nothing
    }

-- | The application ID.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiApplicationId :: Lens.Lens' ApplicationInfo (Core.Maybe Types.ApplicationId)
aiApplicationId = Lens.field @"applicationId"
{-# DEPRECATED aiApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiApplicationName :: Lens.Lens' ApplicationInfo (Core.Maybe Types.ApplicationName)
aiApplicationName = Lens.field @"applicationName"
{-# DEPRECATED aiApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The destination platform type for deployment of the application (@Lambda@ or @Server@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiComputePlatform :: Lens.Lens' ApplicationInfo (Core.Maybe Types.ComputePlatform)
aiComputePlatform = Lens.field @"computePlatform"
{-# DEPRECATED aiComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The time at which the application was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiCreateTime :: Lens.Lens' ApplicationInfo (Core.Maybe Core.NominalDiffTime)
aiCreateTime = Lens.field @"createTime"
{-# DEPRECATED aiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The name for a connection to a GitHub account.
--
-- /Note:/ Consider using 'gitHubAccountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiGitHubAccountName :: Lens.Lens' ApplicationInfo (Core.Maybe Types.GitHubAccountName)
aiGitHubAccountName = Lens.field @"gitHubAccountName"
{-# DEPRECATED aiGitHubAccountName "Use generic-lens or generic-optics with 'gitHubAccountName' instead." #-}

-- | True if the user has authenticated with GitHub for the specified application. Otherwise, false.
--
-- /Note:/ Consider using 'linkedToGitHub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiLinkedToGitHub :: Lens.Lens' ApplicationInfo (Core.Maybe Core.Bool)
aiLinkedToGitHub = Lens.field @"linkedToGitHub"
{-# DEPRECATED aiLinkedToGitHub "Use generic-lens or generic-optics with 'linkedToGitHub' instead." #-}

instance Core.FromJSON ApplicationInfo where
  parseJSON =
    Core.withObject "ApplicationInfo" Core.$
      \x ->
        ApplicationInfo'
          Core.<$> (x Core..:? "applicationId")
          Core.<*> (x Core..:? "applicationName")
          Core.<*> (x Core..:? "computePlatform")
          Core.<*> (x Core..:? "createTime")
          Core.<*> (x Core..:? "gitHubAccountName")
          Core.<*> (x Core..:? "linkedToGitHub")

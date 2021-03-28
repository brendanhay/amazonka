{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.GenericRevisionInfo
  ( GenericRevisionInfo (..)
  -- * Smart constructor
  , mkGenericRevisionInfo
  -- * Lenses
  , griDeploymentGroups
  , griDescription
  , griFirstUsedTime
  , griLastUsedTime
  , griRegisterTime
  ) where

import qualified Network.AWS.CodeDeploy.Types.DeploymentGroupName as Types
import qualified Network.AWS.CodeDeploy.Types.Description as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an application revision.
--
-- /See:/ 'mkGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { deploymentGroups :: Core.Maybe [Types.DeploymentGroupName]
    -- ^ The deployment groups for which this is the current target revision.
  , description :: Core.Maybe Types.Description
    -- ^ A comment about the revision.
  , firstUsedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the revision was first used by AWS CodeDeploy.
  , lastUsedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the revision was last used by AWS CodeDeploy.
  , registerTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the revision was registered with AWS CodeDeploy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GenericRevisionInfo' value with any optional fields omitted.
mkGenericRevisionInfo
    :: GenericRevisionInfo
mkGenericRevisionInfo
  = GenericRevisionInfo'{deploymentGroups = Core.Nothing,
                         description = Core.Nothing, firstUsedTime = Core.Nothing,
                         lastUsedTime = Core.Nothing, registerTime = Core.Nothing}

-- | The deployment groups for which this is the current target revision.
--
-- /Note:/ Consider using 'deploymentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griDeploymentGroups :: Lens.Lens' GenericRevisionInfo (Core.Maybe [Types.DeploymentGroupName])
griDeploymentGroups = Lens.field @"deploymentGroups"
{-# INLINEABLE griDeploymentGroups #-}
{-# DEPRECATED deploymentGroups "Use generic-lens or generic-optics with 'deploymentGroups' instead"  #-}

-- | A comment about the revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griDescription :: Lens.Lens' GenericRevisionInfo (Core.Maybe Types.Description)
griDescription = Lens.field @"description"
{-# INLINEABLE griDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | When the revision was first used by AWS CodeDeploy.
--
-- /Note:/ Consider using 'firstUsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griFirstUsedTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.NominalDiffTime)
griFirstUsedTime = Lens.field @"firstUsedTime"
{-# INLINEABLE griFirstUsedTime #-}
{-# DEPRECATED firstUsedTime "Use generic-lens or generic-optics with 'firstUsedTime' instead"  #-}

-- | When the revision was last used by AWS CodeDeploy.
--
-- /Note:/ Consider using 'lastUsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griLastUsedTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.NominalDiffTime)
griLastUsedTime = Lens.field @"lastUsedTime"
{-# INLINEABLE griLastUsedTime #-}
{-# DEPRECATED lastUsedTime "Use generic-lens or generic-optics with 'lastUsedTime' instead"  #-}

-- | When the revision was registered with AWS CodeDeploy.
--
-- /Note:/ Consider using 'registerTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
griRegisterTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.NominalDiffTime)
griRegisterTime = Lens.field @"registerTime"
{-# INLINEABLE griRegisterTime #-}
{-# DEPRECATED registerTime "Use generic-lens or generic-optics with 'registerTime' instead"  #-}

instance Core.FromJSON GenericRevisionInfo where
        parseJSON
          = Core.withObject "GenericRevisionInfo" Core.$
              \ x ->
                GenericRevisionInfo' Core.<$>
                  (x Core..:? "deploymentGroups") Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "firstUsedTime"
                    Core.<*> x Core..:? "lastUsedTime"
                    Core.<*> x Core..:? "registerTime"

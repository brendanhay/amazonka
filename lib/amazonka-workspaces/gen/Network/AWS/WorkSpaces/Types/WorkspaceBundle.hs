{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceBundle
  ( WorkspaceBundle (..)
  -- * Smart constructor
  , mkWorkspaceBundle
  -- * Lenses
  , wbBundleId
  , wbComputeType
  , wbDescription
  , wbImageId
  , wbLastUpdatedTime
  , wbName
  , wbOwner
  , wbRootStorage
  , wbUserStorage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.BundleId as Types
import qualified Network.AWS.WorkSpaces.Types.BundleOwner as Types
import qualified Network.AWS.WorkSpaces.Types.ComputeType as Types
import qualified Network.AWS.WorkSpaces.Types.Description as Types
import qualified Network.AWS.WorkSpaces.Types.NonEmptyString as Types
import qualified Network.AWS.WorkSpaces.Types.RootStorage as Types
import qualified Network.AWS.WorkSpaces.Types.UserStorage as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceImageId as Types

-- | Describes a WorkSpace bundle.
--
-- /See:/ 'mkWorkspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
  { bundleId :: Core.Maybe Types.BundleId
    -- ^ The bundle identifier.
  , computeType :: Core.Maybe Types.ComputeType
    -- ^ The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
  , description :: Core.Maybe Types.Description
    -- ^ A description.
  , imageId :: Core.Maybe Types.WorkspaceImageId
    -- ^ The image identifier of the bundle.
  , lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time that the bundle was updated.
  , name :: Core.Maybe Types.NonEmptyString
    -- ^ The name of the bundle.
  , owner :: Core.Maybe Types.BundleOwner
    -- ^ The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
  , rootStorage :: Core.Maybe Types.RootStorage
    -- ^ The size of the root volume.
  , userStorage :: Core.Maybe Types.UserStorage
    -- ^ The size of the user storage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WorkspaceBundle' value with any optional fields omitted.
mkWorkspaceBundle
    :: WorkspaceBundle
mkWorkspaceBundle
  = WorkspaceBundle'{bundleId = Core.Nothing,
                     computeType = Core.Nothing, description = Core.Nothing,
                     imageId = Core.Nothing, lastUpdatedTime = Core.Nothing,
                     name = Core.Nothing, owner = Core.Nothing,
                     rootStorage = Core.Nothing, userStorage = Core.Nothing}

-- | The bundle identifier.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbBundleId :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.BundleId)
wbBundleId = Lens.field @"bundleId"
{-# INLINEABLE wbBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbComputeType :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.ComputeType)
wbComputeType = Lens.field @"computeType"
{-# INLINEABLE wbComputeType #-}
{-# DEPRECATED computeType "Use generic-lens or generic-optics with 'computeType' instead"  #-}

-- | A description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbDescription :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.Description)
wbDescription = Lens.field @"description"
{-# INLINEABLE wbDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The image identifier of the bundle.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbImageId :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.WorkspaceImageId)
wbImageId = Lens.field @"imageId"
{-# INLINEABLE wbImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The last time that the bundle was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbLastUpdatedTime :: Lens.Lens' WorkspaceBundle (Core.Maybe Core.NominalDiffTime)
wbLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE wbLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | The name of the bundle.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbName :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.NonEmptyString)
wbName = Lens.field @"name"
{-# INLINEABLE wbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The owner of the bundle. This is the account identifier of the owner, or @AMAZON@ if the bundle is provided by AWS.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbOwner :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.BundleOwner)
wbOwner = Lens.field @"owner"
{-# INLINEABLE wbOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The size of the root volume.
--
-- /Note:/ Consider using 'rootStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbRootStorage :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.RootStorage)
wbRootStorage = Lens.field @"rootStorage"
{-# INLINEABLE wbRootStorage #-}
{-# DEPRECATED rootStorage "Use generic-lens or generic-optics with 'rootStorage' instead"  #-}

-- | The size of the user storage.
--
-- /Note:/ Consider using 'userStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbUserStorage :: Lens.Lens' WorkspaceBundle (Core.Maybe Types.UserStorage)
wbUserStorage = Lens.field @"userStorage"
{-# INLINEABLE wbUserStorage #-}
{-# DEPRECATED userStorage "Use generic-lens or generic-optics with 'userStorage' instead"  #-}

instance Core.FromJSON WorkspaceBundle where
        parseJSON
          = Core.withObject "WorkspaceBundle" Core.$
              \ x ->
                WorkspaceBundle' Core.<$>
                  (x Core..:? "BundleId") Core.<*> x Core..:? "ComputeType" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "ImageId"
                    Core.<*> x Core..:? "LastUpdatedTime"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Owner"
                    Core.<*> x Core..:? "RootStorage"
                    Core.<*> x Core..:? "UserStorage"

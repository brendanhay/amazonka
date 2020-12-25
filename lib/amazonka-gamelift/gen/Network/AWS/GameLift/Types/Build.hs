{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Build
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Build
  ( Build (..),

    -- * Smart constructor
    mkBuild,

    -- * Lenses
    bBuildArn,
    bBuildId,
    bCreationTime,
    bName,
    bOperatingSystem,
    bSizeOnDisk,
    bStatus,
    bVersion,
  )
where

import qualified Network.AWS.GameLift.Types.BuildArn as Types
import qualified Network.AWS.GameLift.Types.BuildId as Types
import qualified Network.AWS.GameLift.Types.BuildStatus as Types
import qualified Network.AWS.GameLift.Types.Name as Types
import qualified Network.AWS.GameLift.Types.OperatingSystem as Types
import qualified Network.AWS.GameLift.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Properties describing a custom game build.
--
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
--
--
--
-- /See:/ 'mkBuild' smart constructor.
data Build = Build'
  { -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift build resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
    buildArn :: Core.Maybe Types.BuildArn,
    -- | A unique identifier for a build.
    buildId :: Core.Maybe Types.BuildId,
    -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | A descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
    name :: Core.Maybe Types.Name,
    -- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
    sizeOnDisk :: Core.Maybe Core.Natural,
    -- | Current status of the build.
    --
    -- Possible build statuses include the following:
    --
    --     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
    --
    --
    --     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
    --
    --
    --     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
    status :: Core.Maybe Types.BuildStatus,
    -- | Version information that is associated with a build or script. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Build' value with any optional fields omitted.
mkBuild ::
  Build
mkBuild =
  Build'
    { buildArn = Core.Nothing,
      buildId = Core.Nothing,
      creationTime = Core.Nothing,
      name = Core.Nothing,
      operatingSystem = Core.Nothing,
      sizeOnDisk = Core.Nothing,
      status = Core.Nothing,
      version = Core.Nothing
    }

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift build resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- /Note:/ Consider using 'buildArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildArn :: Lens.Lens' Build (Core.Maybe Types.BuildArn)
bBuildArn = Lens.field @"buildArn"
{-# DEPRECATED bBuildArn "Use generic-lens or generic-optics with 'buildArn' instead." #-}

-- | A unique identifier for a build.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildId :: Lens.Lens' Build (Core.Maybe Types.BuildId)
bBuildId = Lens.field @"buildId"
{-# DEPRECATED bBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreationTime :: Lens.Lens' Build (Core.Maybe Core.NominalDiffTime)
bCreationTime = Lens.field @"creationTime"
{-# DEPRECATED bCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Build (Core.Maybe Types.Name)
bName = Lens.field @"name"
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bOperatingSystem :: Lens.Lens' Build (Core.Maybe Types.OperatingSystem)
bOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED bOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
--
-- /Note:/ Consider using 'sizeOnDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSizeOnDisk :: Lens.Lens' Build (Core.Maybe Core.Natural)
bSizeOnDisk = Lens.field @"sizeOnDisk"
{-# DEPRECATED bSizeOnDisk "Use generic-lens or generic-optics with 'sizeOnDisk' instead." #-}

-- | Current status of the build.
--
-- Possible build statuses include the following:
--
--     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
--
--
--     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
--
--
--     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStatus :: Lens.Lens' Build (Core.Maybe Types.BuildStatus)
bStatus = Lens.field @"status"
{-# DEPRECATED bStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersion :: Lens.Lens' Build (Core.Maybe Types.Version)
bVersion = Lens.field @"version"
{-# DEPRECATED bVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON Build where
  parseJSON =
    Core.withObject "Build" Core.$
      \x ->
        Build'
          Core.<$> (x Core..:? "BuildArn")
          Core.<*> (x Core..:? "BuildId")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "OperatingSystem")
          Core.<*> (x Core..:? "SizeOnDisk")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Version")

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
    bCreationTime,
    bStatus,
    bOperatingSystem,
    bBuildId,
    bName,
    bVersion,
    bBuildARN,
    bSizeOnDisk,
  )
where

import Network.AWS.GameLift.Types.BuildStatus
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Lude.Maybe Lude.Timestamp,
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
    status :: Lude.Maybe BuildStatus,
    -- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A unique identifier for a build.
    buildId :: Lude.Maybe Lude.Text,
    -- | A descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
    name :: Lude.Maybe Lude.Text,
    -- | Version information that is associated with a build or script. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
    version :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift build resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
    buildARN :: Lude.Maybe Lude.Text,
    -- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
    sizeOnDisk :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- * 'creationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'status' - Current status of the build.
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
-- * 'operatingSystem' - Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
-- * 'buildId' - A unique identifier for a build.
-- * 'name' - A descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
-- * 'version' - Version information that is associated with a build or script. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
-- * 'buildARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift build resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
-- * 'sizeOnDisk' - File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
mkBuild ::
  Build
mkBuild =
  Build'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      buildId = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      buildARN = Lude.Nothing,
      sizeOnDisk = Lude.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreationTime :: Lens.Lens' Build (Lude.Maybe Lude.Timestamp)
bCreationTime = Lens.lens (creationTime :: Build -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Build)
{-# DEPRECATED bCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
bStatus :: Lens.Lens' Build (Lude.Maybe BuildStatus)
bStatus = Lens.lens (status :: Build -> Lude.Maybe BuildStatus) (\s a -> s {status = a} :: Build)
{-# DEPRECATED bStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bOperatingSystem :: Lens.Lens' Build (Lude.Maybe OperatingSystem)
bOperatingSystem = Lens.lens (operatingSystem :: Build -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: Build)
{-# DEPRECATED bOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A unique identifier for a build.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildId :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bBuildId = Lens.lens (buildId :: Build -> Lude.Maybe Lude.Text) (\s a -> s {buildId = a} :: Build)
{-# DEPRECATED bBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | A descriptive label that is associated with a build. Build names do not need to be unique. It can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bName = Lens.lens (name :: Build -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Build)
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique. This value can be set using 'CreateBuild' or 'UpdateBuild' .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersion :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bVersion = Lens.lens (version :: Build -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Build)
{-# DEPRECATED bVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift build resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- /Note:/ Consider using 'buildARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildARN :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bBuildARN = Lens.lens (buildARN :: Build -> Lude.Maybe Lude.Text) (\s a -> s {buildARN = a} :: Build)
{-# DEPRECATED bBuildARN "Use generic-lens or generic-optics with 'buildARN' instead." #-}

-- | File size of the uploaded game build, expressed in bytes. When the build status is @INITIALIZED@ , this value is 0.
--
-- /Note:/ Consider using 'sizeOnDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSizeOnDisk :: Lens.Lens' Build (Lude.Maybe Lude.Natural)
bSizeOnDisk = Lens.lens (sizeOnDisk :: Build -> Lude.Maybe Lude.Natural) (\s a -> s {sizeOnDisk = a} :: Build)
{-# DEPRECATED bSizeOnDisk "Use generic-lens or generic-optics with 'sizeOnDisk' instead." #-}

instance Lude.FromJSON Build where
  parseJSON =
    Lude.withObject
      "Build"
      ( \x ->
          Build'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "BuildId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "BuildArn")
            Lude.<*> (x Lude..:? "SizeOnDisk")
      )

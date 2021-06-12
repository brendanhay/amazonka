{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Build
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Build where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.BuildStatus
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens

-- | Properties describing a custom game build.
--
-- __Related operations__
--
-- -   CreateBuild
--
-- -   ListBuilds
--
-- -   DescribeBuild
--
-- -   UpdateBuild
--
-- -   DeleteBuild
--
-- /See:/ 'newBuild' smart constructor.
data Build = Build'
  { -- | Current status of the build.
    --
    -- Possible build statuses include the following:
    --
    -- -   __INITIALIZED__ -- A new build has been defined, but no files have
    --     been uploaded. You cannot create fleets for builds that are in this
    --     status. When a build is successfully created, the build status is
    --     set to this value.
    --
    -- -   __READY__ -- The game build has been successfully uploaded. You can
    --     now create new fleets for this build.
    --
    -- -   __FAILED__ -- The game build upload failed. You cannot create new
    --     fleets for this build.
    status :: Core.Maybe BuildStatus,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique. This value can be set using
    -- CreateBuild or UpdateBuild.
    version :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a build. Build names do not
    -- need to be unique. It can be set using CreateBuild or UpdateBuild.
    name :: Core.Maybe Core.Text,
    -- | File size of the uploaded game build, expressed in bytes. When the build
    -- status is @INITIALIZED@, this value is 0.
    sizeOnDisk :: Core.Maybe Core.Natural,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift build resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift build ARN, the
    -- resource ID matches the /BuildId/ value.
    buildArn :: Core.Maybe Core.Text,
    -- | A unique identifier for a build.
    buildId :: Core.Maybe Core.Text,
    -- | Operating system that the game server binaries are built to run on. This
    -- value determines the type of fleet resources that you can use for this
    -- build.
    operatingSystem :: Core.Maybe OperatingSystem
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Build' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'build_status' - Current status of the build.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
--
-- 'creationTime', 'build_creationTime' - Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'version', 'build_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique. This value can be set using
-- CreateBuild or UpdateBuild.
--
-- 'name', 'build_name' - A descriptive label that is associated with a build. Build names do not
-- need to be unique. It can be set using CreateBuild or UpdateBuild.
--
-- 'sizeOnDisk', 'build_sizeOnDisk' - File size of the uploaded game build, expressed in bytes. When the build
-- status is @INITIALIZED@, this value is 0.
--
-- 'buildArn', 'build_buildArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift build resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift build ARN, the
-- resource ID matches the /BuildId/ value.
--
-- 'buildId', 'build_buildId' - A unique identifier for a build.
--
-- 'operatingSystem', 'build_operatingSystem' - Operating system that the game server binaries are built to run on. This
-- value determines the type of fleet resources that you can use for this
-- build.
newBuild ::
  Build
newBuild =
  Build'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      sizeOnDisk = Core.Nothing,
      buildArn = Core.Nothing,
      buildId = Core.Nothing,
      operatingSystem = Core.Nothing
    }

-- | Current status of the build.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
build_status :: Lens.Lens' Build (Core.Maybe BuildStatus)
build_status = Lens.lens (\Build' {status} -> status) (\s@Build' {} a -> s {status = a} :: Build)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
build_creationTime :: Lens.Lens' Build (Core.Maybe Core.UTCTime)
build_creationTime = Lens.lens (\Build' {creationTime} -> creationTime) (\s@Build' {} a -> s {creationTime = a} :: Build) Core.. Lens.mapping Core._Time

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique. This value can be set using
-- CreateBuild or UpdateBuild.
build_version :: Lens.Lens' Build (Core.Maybe Core.Text)
build_version = Lens.lens (\Build' {version} -> version) (\s@Build' {} a -> s {version = a} :: Build)

-- | A descriptive label that is associated with a build. Build names do not
-- need to be unique. It can be set using CreateBuild or UpdateBuild.
build_name :: Lens.Lens' Build (Core.Maybe Core.Text)
build_name = Lens.lens (\Build' {name} -> name) (\s@Build' {} a -> s {name = a} :: Build)

-- | File size of the uploaded game build, expressed in bytes. When the build
-- status is @INITIALIZED@, this value is 0.
build_sizeOnDisk :: Lens.Lens' Build (Core.Maybe Core.Natural)
build_sizeOnDisk = Lens.lens (\Build' {sizeOnDisk} -> sizeOnDisk) (\s@Build' {} a -> s {sizeOnDisk = a} :: Build)

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift build resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift build ARN, the
-- resource ID matches the /BuildId/ value.
build_buildArn :: Lens.Lens' Build (Core.Maybe Core.Text)
build_buildArn = Lens.lens (\Build' {buildArn} -> buildArn) (\s@Build' {} a -> s {buildArn = a} :: Build)

-- | A unique identifier for a build.
build_buildId :: Lens.Lens' Build (Core.Maybe Core.Text)
build_buildId = Lens.lens (\Build' {buildId} -> buildId) (\s@Build' {} a -> s {buildId = a} :: Build)

-- | Operating system that the game server binaries are built to run on. This
-- value determines the type of fleet resources that you can use for this
-- build.
build_operatingSystem :: Lens.Lens' Build (Core.Maybe OperatingSystem)
build_operatingSystem = Lens.lens (\Build' {operatingSystem} -> operatingSystem) (\s@Build' {} a -> s {operatingSystem = a} :: Build)

instance Core.FromJSON Build where
  parseJSON =
    Core.withObject
      "Build"
      ( \x ->
          Build'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "SizeOnDisk")
            Core.<*> (x Core..:? "BuildArn")
            Core.<*> (x Core..:? "BuildId")
            Core.<*> (x Core..:? "OperatingSystem")
      )

instance Core.Hashable Build

instance Core.NFData Build

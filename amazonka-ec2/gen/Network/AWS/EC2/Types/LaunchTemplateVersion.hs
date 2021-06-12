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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResponseLaunchTemplateData
import qualified Network.AWS.Lens as Lens

-- | Describes a launch template version.
--
-- /See:/ 'newLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { -- | Indicates whether the version is the default version.
    defaultVersion :: Core.Maybe Core.Bool,
    -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | Information about the launch template.
    launchTemplateData :: Core.Maybe ResponseLaunchTemplateData,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | The description for the version.
    versionDescription :: Core.Maybe Core.Text,
    -- | The version number.
    versionNumber :: Core.Maybe Core.Integer,
    -- | The time the version was created.
    createTime :: Core.Maybe Core.ISO8601,
    -- | The principal that created the version.
    createdBy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersion', 'launchTemplateVersion_defaultVersion' - Indicates whether the version is the default version.
--
-- 'launchTemplateId', 'launchTemplateVersion_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateData', 'launchTemplateVersion_launchTemplateData' - Information about the launch template.
--
-- 'launchTemplateName', 'launchTemplateVersion_launchTemplateName' - The name of the launch template.
--
-- 'versionDescription', 'launchTemplateVersion_versionDescription' - The description for the version.
--
-- 'versionNumber', 'launchTemplateVersion_versionNumber' - The version number.
--
-- 'createTime', 'launchTemplateVersion_createTime' - The time the version was created.
--
-- 'createdBy', 'launchTemplateVersion_createdBy' - The principal that created the version.
newLaunchTemplateVersion ::
  LaunchTemplateVersion
newLaunchTemplateVersion =
  LaunchTemplateVersion'
    { defaultVersion =
        Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateData = Core.Nothing,
      launchTemplateName = Core.Nothing,
      versionDescription = Core.Nothing,
      versionNumber = Core.Nothing,
      createTime = Core.Nothing,
      createdBy = Core.Nothing
    }

-- | Indicates whether the version is the default version.
launchTemplateVersion_defaultVersion :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Bool)
launchTemplateVersion_defaultVersion = Lens.lens (\LaunchTemplateVersion' {defaultVersion} -> defaultVersion) (\s@LaunchTemplateVersion' {} a -> s {defaultVersion = a} :: LaunchTemplateVersion)

-- | The ID of the launch template.
launchTemplateVersion_launchTemplateId :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
launchTemplateVersion_launchTemplateId = Lens.lens (\LaunchTemplateVersion' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateId = a} :: LaunchTemplateVersion)

-- | Information about the launch template.
launchTemplateVersion_launchTemplateData :: Lens.Lens' LaunchTemplateVersion (Core.Maybe ResponseLaunchTemplateData)
launchTemplateVersion_launchTemplateData = Lens.lens (\LaunchTemplateVersion' {launchTemplateData} -> launchTemplateData) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateData = a} :: LaunchTemplateVersion)

-- | The name of the launch template.
launchTemplateVersion_launchTemplateName :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
launchTemplateVersion_launchTemplateName = Lens.lens (\LaunchTemplateVersion' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateName = a} :: LaunchTemplateVersion)

-- | The description for the version.
launchTemplateVersion_versionDescription :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
launchTemplateVersion_versionDescription = Lens.lens (\LaunchTemplateVersion' {versionDescription} -> versionDescription) (\s@LaunchTemplateVersion' {} a -> s {versionDescription = a} :: LaunchTemplateVersion)

-- | The version number.
launchTemplateVersion_versionNumber :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Integer)
launchTemplateVersion_versionNumber = Lens.lens (\LaunchTemplateVersion' {versionNumber} -> versionNumber) (\s@LaunchTemplateVersion' {} a -> s {versionNumber = a} :: LaunchTemplateVersion)

-- | The time the version was created.
launchTemplateVersion_createTime :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.UTCTime)
launchTemplateVersion_createTime = Lens.lens (\LaunchTemplateVersion' {createTime} -> createTime) (\s@LaunchTemplateVersion' {} a -> s {createTime = a} :: LaunchTemplateVersion) Core.. Lens.mapping Core._Time

-- | The principal that created the version.
launchTemplateVersion_createdBy :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Text)
launchTemplateVersion_createdBy = Lens.lens (\LaunchTemplateVersion' {createdBy} -> createdBy) (\s@LaunchTemplateVersion' {} a -> s {createdBy = a} :: LaunchTemplateVersion)

instance Core.FromXML LaunchTemplateVersion where
  parseXML x =
    LaunchTemplateVersion'
      Core.<$> (x Core..@? "defaultVersion")
      Core.<*> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateData")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> (x Core..@? "versionDescription")
      Core.<*> (x Core..@? "versionNumber")
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "createdBy")

instance Core.Hashable LaunchTemplateVersion

instance Core.NFData LaunchTemplateVersion

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
-- Module      : Amazonka.EC2.Types.LaunchTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateVersion where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResponseLaunchTemplateData
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template version.
--
-- /See:/ 'newLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The principal that created the version.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the version is the default version.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The version number.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The description for the version.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | Information about the launch template.
    launchTemplateData :: Prelude.Maybe ResponseLaunchTemplateData,
    -- | The time the version was created.
    createTime :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateName', 'launchTemplateVersion_launchTemplateName' - The name of the launch template.
--
-- 'launchTemplateId', 'launchTemplateVersion_launchTemplateId' - The ID of the launch template.
--
-- 'createdBy', 'launchTemplateVersion_createdBy' - The principal that created the version.
--
-- 'defaultVersion', 'launchTemplateVersion_defaultVersion' - Indicates whether the version is the default version.
--
-- 'versionNumber', 'launchTemplateVersion_versionNumber' - The version number.
--
-- 'versionDescription', 'launchTemplateVersion_versionDescription' - The description for the version.
--
-- 'launchTemplateData', 'launchTemplateVersion_launchTemplateData' - Information about the launch template.
--
-- 'createTime', 'launchTemplateVersion_createTime' - The time the version was created.
newLaunchTemplateVersion ::
  LaunchTemplateVersion
newLaunchTemplateVersion =
  LaunchTemplateVersion'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      versionDescription = Prelude.Nothing,
      launchTemplateData = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | The name of the launch template.
launchTemplateVersion_launchTemplateName :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_launchTemplateName = Lens.lens (\LaunchTemplateVersion' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateName = a} :: LaunchTemplateVersion)

-- | The ID of the launch template.
launchTemplateVersion_launchTemplateId :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_launchTemplateId = Lens.lens (\LaunchTemplateVersion' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateId = a} :: LaunchTemplateVersion)

-- | The principal that created the version.
launchTemplateVersion_createdBy :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_createdBy = Lens.lens (\LaunchTemplateVersion' {createdBy} -> createdBy) (\s@LaunchTemplateVersion' {} a -> s {createdBy = a} :: LaunchTemplateVersion)

-- | Indicates whether the version is the default version.
launchTemplateVersion_defaultVersion :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Bool)
launchTemplateVersion_defaultVersion = Lens.lens (\LaunchTemplateVersion' {defaultVersion} -> defaultVersion) (\s@LaunchTemplateVersion' {} a -> s {defaultVersion = a} :: LaunchTemplateVersion)

-- | The version number.
launchTemplateVersion_versionNumber :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Integer)
launchTemplateVersion_versionNumber = Lens.lens (\LaunchTemplateVersion' {versionNumber} -> versionNumber) (\s@LaunchTemplateVersion' {} a -> s {versionNumber = a} :: LaunchTemplateVersion)

-- | The description for the version.
launchTemplateVersion_versionDescription :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_versionDescription = Lens.lens (\LaunchTemplateVersion' {versionDescription} -> versionDescription) (\s@LaunchTemplateVersion' {} a -> s {versionDescription = a} :: LaunchTemplateVersion)

-- | Information about the launch template.
launchTemplateVersion_launchTemplateData :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe ResponseLaunchTemplateData)
launchTemplateVersion_launchTemplateData = Lens.lens (\LaunchTemplateVersion' {launchTemplateData} -> launchTemplateData) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateData = a} :: LaunchTemplateVersion)

-- | The time the version was created.
launchTemplateVersion_createTime :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.UTCTime)
launchTemplateVersion_createTime = Lens.lens (\LaunchTemplateVersion' {createTime} -> createTime) (\s@LaunchTemplateVersion' {} a -> s {createTime = a} :: LaunchTemplateVersion) Prelude.. Lens.mapping Core._Time

instance Core.FromXML LaunchTemplateVersion where
  parseXML x =
    LaunchTemplateVersion'
      Prelude.<$> (x Core..@? "launchTemplateName")
      Prelude.<*> (x Core..@? "launchTemplateId")
      Prelude.<*> (x Core..@? "createdBy")
      Prelude.<*> (x Core..@? "defaultVersion")
      Prelude.<*> (x Core..@? "versionNumber")
      Prelude.<*> (x Core..@? "versionDescription")
      Prelude.<*> (x Core..@? "launchTemplateData")
      Prelude.<*> (x Core..@? "createTime")

instance Prelude.Hashable LaunchTemplateVersion where
  hashWithSalt _salt LaunchTemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` launchTemplateData
      `Prelude.hashWithSalt` createTime

instance Prelude.NFData LaunchTemplateVersion where
  rnf LaunchTemplateVersion' {..} =
    Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf launchTemplateData
      `Prelude.seq` Prelude.rnf createTime

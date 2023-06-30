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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResponseLaunchTemplateData
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template version.
--
-- /See:/ 'newLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { -- | The time the version was created.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | The principal that created the version.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the version is the default version.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | Information about the launch template.
    launchTemplateData :: Prelude.Maybe ResponseLaunchTemplateData,
    -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The description for the version.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Maybe Prelude.Integer
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
-- 'createTime', 'launchTemplateVersion_createTime' - The time the version was created.
--
-- 'createdBy', 'launchTemplateVersion_createdBy' - The principal that created the version.
--
-- 'defaultVersion', 'launchTemplateVersion_defaultVersion' - Indicates whether the version is the default version.
--
-- 'launchTemplateData', 'launchTemplateVersion_launchTemplateData' - Information about the launch template.
--
-- 'launchTemplateId', 'launchTemplateVersion_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'launchTemplateVersion_launchTemplateName' - The name of the launch template.
--
-- 'versionDescription', 'launchTemplateVersion_versionDescription' - The description for the version.
--
-- 'versionNumber', 'launchTemplateVersion_versionNumber' - The version number.
newLaunchTemplateVersion ::
  LaunchTemplateVersion
newLaunchTemplateVersion =
  LaunchTemplateVersion'
    { createTime =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      launchTemplateData = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      versionDescription = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The time the version was created.
launchTemplateVersion_createTime :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.UTCTime)
launchTemplateVersion_createTime = Lens.lens (\LaunchTemplateVersion' {createTime} -> createTime) (\s@LaunchTemplateVersion' {} a -> s {createTime = a} :: LaunchTemplateVersion) Prelude.. Lens.mapping Data._Time

-- | The principal that created the version.
launchTemplateVersion_createdBy :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_createdBy = Lens.lens (\LaunchTemplateVersion' {createdBy} -> createdBy) (\s@LaunchTemplateVersion' {} a -> s {createdBy = a} :: LaunchTemplateVersion)

-- | Indicates whether the version is the default version.
launchTemplateVersion_defaultVersion :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Bool)
launchTemplateVersion_defaultVersion = Lens.lens (\LaunchTemplateVersion' {defaultVersion} -> defaultVersion) (\s@LaunchTemplateVersion' {} a -> s {defaultVersion = a} :: LaunchTemplateVersion)

-- | Information about the launch template.
launchTemplateVersion_launchTemplateData :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe ResponseLaunchTemplateData)
launchTemplateVersion_launchTemplateData = Lens.lens (\LaunchTemplateVersion' {launchTemplateData} -> launchTemplateData) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateData = a} :: LaunchTemplateVersion)

-- | The ID of the launch template.
launchTemplateVersion_launchTemplateId :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_launchTemplateId = Lens.lens (\LaunchTemplateVersion' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateId = a} :: LaunchTemplateVersion)

-- | The name of the launch template.
launchTemplateVersion_launchTemplateName :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_launchTemplateName = Lens.lens (\LaunchTemplateVersion' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateVersion' {} a -> s {launchTemplateName = a} :: LaunchTemplateVersion)

-- | The description for the version.
launchTemplateVersion_versionDescription :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Text)
launchTemplateVersion_versionDescription = Lens.lens (\LaunchTemplateVersion' {versionDescription} -> versionDescription) (\s@LaunchTemplateVersion' {} a -> s {versionDescription = a} :: LaunchTemplateVersion)

-- | The version number.
launchTemplateVersion_versionNumber :: Lens.Lens' LaunchTemplateVersion (Prelude.Maybe Prelude.Integer)
launchTemplateVersion_versionNumber = Lens.lens (\LaunchTemplateVersion' {versionNumber} -> versionNumber) (\s@LaunchTemplateVersion' {} a -> s {versionNumber = a} :: LaunchTemplateVersion)

instance Data.FromXML LaunchTemplateVersion where
  parseXML x =
    LaunchTemplateVersion'
      Prelude.<$> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "createdBy")
      Prelude.<*> (x Data..@? "defaultVersion")
      Prelude.<*> (x Data..@? "launchTemplateData")
      Prelude.<*> (x Data..@? "launchTemplateId")
      Prelude.<*> (x Data..@? "launchTemplateName")
      Prelude.<*> (x Data..@? "versionDescription")
      Prelude.<*> (x Data..@? "versionNumber")

instance Prelude.Hashable LaunchTemplateVersion where
  hashWithSalt _salt LaunchTemplateVersion' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` launchTemplateData
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData LaunchTemplateVersion where
  rnf LaunchTemplateVersion' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf launchTemplateData
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf versionNumber

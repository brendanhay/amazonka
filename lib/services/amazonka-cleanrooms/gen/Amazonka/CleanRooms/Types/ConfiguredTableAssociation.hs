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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configured table association links a configured table to a
-- collaboration.
--
-- /See:/ 'newConfiguredTableAssociation' smart constructor.
data ConfiguredTableAssociation = ConfiguredTableAssociation'
  { -- | A description of the configured table association.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ARN for the configured table association.
    arn :: Prelude.Text,
    -- | The unique ID for the configured table association.
    id :: Prelude.Text,
    -- | The unique ID for the configured table that the association refers to.
    configuredTableId :: Prelude.Text,
    -- | The unique ARN for the configured table that the association refers to.
    configuredTableArn :: Prelude.Text,
    -- | The unique ID for the membership this configured table association
    -- belongs to.
    membershipId :: Prelude.Text,
    -- | The unique ARN for the membership this configured table association
    -- belongs to.
    membershipArn :: Prelude.Text,
    -- | The service will assume this role to access catalog metadata and query
    -- the table.
    roleArn :: Prelude.Text,
    -- | The name of the configured table association, in lowercase. The table is
    -- identified by this name when running protected queries against the
    -- underlying data.
    name :: Prelude.Text,
    -- | The time the configured table association was created.
    createTime :: Data.POSIX,
    -- | The time the configured table association was last updated.
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'configuredTableAssociation_description' - A description of the configured table association.
--
-- 'arn', 'configuredTableAssociation_arn' - The unique ARN for the configured table association.
--
-- 'id', 'configuredTableAssociation_id' - The unique ID for the configured table association.
--
-- 'configuredTableId', 'configuredTableAssociation_configuredTableId' - The unique ID for the configured table that the association refers to.
--
-- 'configuredTableArn', 'configuredTableAssociation_configuredTableArn' - The unique ARN for the configured table that the association refers to.
--
-- 'membershipId', 'configuredTableAssociation_membershipId' - The unique ID for the membership this configured table association
-- belongs to.
--
-- 'membershipArn', 'configuredTableAssociation_membershipArn' - The unique ARN for the membership this configured table association
-- belongs to.
--
-- 'roleArn', 'configuredTableAssociation_roleArn' - The service will assume this role to access catalog metadata and query
-- the table.
--
-- 'name', 'configuredTableAssociation_name' - The name of the configured table association, in lowercase. The table is
-- identified by this name when running protected queries against the
-- underlying data.
--
-- 'createTime', 'configuredTableAssociation_createTime' - The time the configured table association was created.
--
-- 'updateTime', 'configuredTableAssociation_updateTime' - The time the configured table association was last updated.
newConfiguredTableAssociation ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'configuredTableId'
  Prelude.Text ->
  -- | 'configuredTableArn'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'membershipArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ConfiguredTableAssociation
newConfiguredTableAssociation
  pArn_
  pId_
  pConfiguredTableId_
  pConfiguredTableArn_
  pMembershipId_
  pMembershipArn_
  pRoleArn_
  pName_
  pCreateTime_
  pUpdateTime_ =
    ConfiguredTableAssociation'
      { description =
          Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        configuredTableId = pConfiguredTableId_,
        configuredTableArn = pConfiguredTableArn_,
        membershipId = pMembershipId_,
        membershipArn = pMembershipArn_,
        roleArn = pRoleArn_,
        name = pName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | A description of the configured table association.
configuredTableAssociation_description :: Lens.Lens' ConfiguredTableAssociation (Prelude.Maybe Prelude.Text)
configuredTableAssociation_description = Lens.lens (\ConfiguredTableAssociation' {description} -> description) (\s@ConfiguredTableAssociation' {} a -> s {description = a} :: ConfiguredTableAssociation)

-- | The unique ARN for the configured table association.
configuredTableAssociation_arn :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_arn = Lens.lens (\ConfiguredTableAssociation' {arn} -> arn) (\s@ConfiguredTableAssociation' {} a -> s {arn = a} :: ConfiguredTableAssociation)

-- | The unique ID for the configured table association.
configuredTableAssociation_id :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_id = Lens.lens (\ConfiguredTableAssociation' {id} -> id) (\s@ConfiguredTableAssociation' {} a -> s {id = a} :: ConfiguredTableAssociation)

-- | The unique ID for the configured table that the association refers to.
configuredTableAssociation_configuredTableId :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_configuredTableId = Lens.lens (\ConfiguredTableAssociation' {configuredTableId} -> configuredTableId) (\s@ConfiguredTableAssociation' {} a -> s {configuredTableId = a} :: ConfiguredTableAssociation)

-- | The unique ARN for the configured table that the association refers to.
configuredTableAssociation_configuredTableArn :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_configuredTableArn = Lens.lens (\ConfiguredTableAssociation' {configuredTableArn} -> configuredTableArn) (\s@ConfiguredTableAssociation' {} a -> s {configuredTableArn = a} :: ConfiguredTableAssociation)

-- | The unique ID for the membership this configured table association
-- belongs to.
configuredTableAssociation_membershipId :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_membershipId = Lens.lens (\ConfiguredTableAssociation' {membershipId} -> membershipId) (\s@ConfiguredTableAssociation' {} a -> s {membershipId = a} :: ConfiguredTableAssociation)

-- | The unique ARN for the membership this configured table association
-- belongs to.
configuredTableAssociation_membershipArn :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_membershipArn = Lens.lens (\ConfiguredTableAssociation' {membershipArn} -> membershipArn) (\s@ConfiguredTableAssociation' {} a -> s {membershipArn = a} :: ConfiguredTableAssociation)

-- | The service will assume this role to access catalog metadata and query
-- the table.
configuredTableAssociation_roleArn :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_roleArn = Lens.lens (\ConfiguredTableAssociation' {roleArn} -> roleArn) (\s@ConfiguredTableAssociation' {} a -> s {roleArn = a} :: ConfiguredTableAssociation)

-- | The name of the configured table association, in lowercase. The table is
-- identified by this name when running protected queries against the
-- underlying data.
configuredTableAssociation_name :: Lens.Lens' ConfiguredTableAssociation Prelude.Text
configuredTableAssociation_name = Lens.lens (\ConfiguredTableAssociation' {name} -> name) (\s@ConfiguredTableAssociation' {} a -> s {name = a} :: ConfiguredTableAssociation)

-- | The time the configured table association was created.
configuredTableAssociation_createTime :: Lens.Lens' ConfiguredTableAssociation Prelude.UTCTime
configuredTableAssociation_createTime = Lens.lens (\ConfiguredTableAssociation' {createTime} -> createTime) (\s@ConfiguredTableAssociation' {} a -> s {createTime = a} :: ConfiguredTableAssociation) Prelude.. Data._Time

-- | The time the configured table association was last updated.
configuredTableAssociation_updateTime :: Lens.Lens' ConfiguredTableAssociation Prelude.UTCTime
configuredTableAssociation_updateTime = Lens.lens (\ConfiguredTableAssociation' {updateTime} -> updateTime) (\s@ConfiguredTableAssociation' {} a -> s {updateTime = a} :: ConfiguredTableAssociation) Prelude.. Data._Time

instance Data.FromJSON ConfiguredTableAssociation where
  parseJSON =
    Data.withObject
      "ConfiguredTableAssociation"
      ( \x ->
          ConfiguredTableAssociation'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "configuredTableId")
            Prelude.<*> (x Data..: "configuredTableArn")
            Prelude.<*> (x Data..: "membershipId")
            Prelude.<*> (x Data..: "membershipArn")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable ConfiguredTableAssociation where
  hashWithSalt _salt ConfiguredTableAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` configuredTableId
      `Prelude.hashWithSalt` configuredTableArn
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` membershipArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ConfiguredTableAssociation where
  rnf ConfiguredTableAssociation' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf configuredTableId
      `Prelude.seq` Prelude.rnf configuredTableArn
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime

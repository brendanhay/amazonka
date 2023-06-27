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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configured table association summary for the objects listed by the
-- request.
--
-- /See:/ 'newConfiguredTableAssociationSummary' smart constructor.
data ConfiguredTableAssociationSummary = ConfiguredTableAssociationSummary'
  { -- | The unique configured table ID that this configured table association
    -- refers to.
    configuredTableId :: Prelude.Text,
    -- | The unique ID for the membership that the configured table association
    -- belongs to.
    membershipId :: Prelude.Text,
    -- | The unique ARN for the membership that the configured table association
    -- belongs to.
    membershipArn :: Prelude.Text,
    -- | The name of the configured table association. The table is identified by
    -- this name when running Protected Queries against the underlying data.
    name :: Prelude.Text,
    -- | The time the configured table association was created.
    createTime :: Data.POSIX,
    -- | The time the configured table association was last updated.
    updateTime :: Data.POSIX,
    -- | The unique ID for the configured table association.
    id :: Prelude.Text,
    -- | The unique ARN for the configured table association.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfiguredTableAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableId', 'configuredTableAssociationSummary_configuredTableId' - The unique configured table ID that this configured table association
-- refers to.
--
-- 'membershipId', 'configuredTableAssociationSummary_membershipId' - The unique ID for the membership that the configured table association
-- belongs to.
--
-- 'membershipArn', 'configuredTableAssociationSummary_membershipArn' - The unique ARN for the membership that the configured table association
-- belongs to.
--
-- 'name', 'configuredTableAssociationSummary_name' - The name of the configured table association. The table is identified by
-- this name when running Protected Queries against the underlying data.
--
-- 'createTime', 'configuredTableAssociationSummary_createTime' - The time the configured table association was created.
--
-- 'updateTime', 'configuredTableAssociationSummary_updateTime' - The time the configured table association was last updated.
--
-- 'id', 'configuredTableAssociationSummary_id' - The unique ID for the configured table association.
--
-- 'arn', 'configuredTableAssociationSummary_arn' - The unique ARN for the configured table association.
newConfiguredTableAssociationSummary ::
  -- | 'configuredTableId'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'membershipArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ConfiguredTableAssociationSummary
newConfiguredTableAssociationSummary
  pConfiguredTableId_
  pMembershipId_
  pMembershipArn_
  pName_
  pCreateTime_
  pUpdateTime_
  pId_
  pArn_ =
    ConfiguredTableAssociationSummary'
      { configuredTableId =
          pConfiguredTableId_,
        membershipId = pMembershipId_,
        membershipArn = pMembershipArn_,
        name = pName_,
        createTime =
          Data._Time Lens.# pCreateTime_,
        updateTime =
          Data._Time Lens.# pUpdateTime_,
        id = pId_,
        arn = pArn_
      }

-- | The unique configured table ID that this configured table association
-- refers to.
configuredTableAssociationSummary_configuredTableId :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_configuredTableId = Lens.lens (\ConfiguredTableAssociationSummary' {configuredTableId} -> configuredTableId) (\s@ConfiguredTableAssociationSummary' {} a -> s {configuredTableId = a} :: ConfiguredTableAssociationSummary)

-- | The unique ID for the membership that the configured table association
-- belongs to.
configuredTableAssociationSummary_membershipId :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_membershipId = Lens.lens (\ConfiguredTableAssociationSummary' {membershipId} -> membershipId) (\s@ConfiguredTableAssociationSummary' {} a -> s {membershipId = a} :: ConfiguredTableAssociationSummary)

-- | The unique ARN for the membership that the configured table association
-- belongs to.
configuredTableAssociationSummary_membershipArn :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_membershipArn = Lens.lens (\ConfiguredTableAssociationSummary' {membershipArn} -> membershipArn) (\s@ConfiguredTableAssociationSummary' {} a -> s {membershipArn = a} :: ConfiguredTableAssociationSummary)

-- | The name of the configured table association. The table is identified by
-- this name when running Protected Queries against the underlying data.
configuredTableAssociationSummary_name :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_name = Lens.lens (\ConfiguredTableAssociationSummary' {name} -> name) (\s@ConfiguredTableAssociationSummary' {} a -> s {name = a} :: ConfiguredTableAssociationSummary)

-- | The time the configured table association was created.
configuredTableAssociationSummary_createTime :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.UTCTime
configuredTableAssociationSummary_createTime = Lens.lens (\ConfiguredTableAssociationSummary' {createTime} -> createTime) (\s@ConfiguredTableAssociationSummary' {} a -> s {createTime = a} :: ConfiguredTableAssociationSummary) Prelude.. Data._Time

-- | The time the configured table association was last updated.
configuredTableAssociationSummary_updateTime :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.UTCTime
configuredTableAssociationSummary_updateTime = Lens.lens (\ConfiguredTableAssociationSummary' {updateTime} -> updateTime) (\s@ConfiguredTableAssociationSummary' {} a -> s {updateTime = a} :: ConfiguredTableAssociationSummary) Prelude.. Data._Time

-- | The unique ID for the configured table association.
configuredTableAssociationSummary_id :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_id = Lens.lens (\ConfiguredTableAssociationSummary' {id} -> id) (\s@ConfiguredTableAssociationSummary' {} a -> s {id = a} :: ConfiguredTableAssociationSummary)

-- | The unique ARN for the configured table association.
configuredTableAssociationSummary_arn :: Lens.Lens' ConfiguredTableAssociationSummary Prelude.Text
configuredTableAssociationSummary_arn = Lens.lens (\ConfiguredTableAssociationSummary' {arn} -> arn) (\s@ConfiguredTableAssociationSummary' {} a -> s {arn = a} :: ConfiguredTableAssociationSummary)

instance
  Data.FromJSON
    ConfiguredTableAssociationSummary
  where
  parseJSON =
    Data.withObject
      "ConfiguredTableAssociationSummary"
      ( \x ->
          ConfiguredTableAssociationSummary'
            Prelude.<$> (x Data..: "configuredTableId")
            Prelude.<*> (x Data..: "membershipId")
            Prelude.<*> (x Data..: "membershipArn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
      )

instance
  Prelude.Hashable
    ConfiguredTableAssociationSummary
  where
  hashWithSalt
    _salt
    ConfiguredTableAssociationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableId
        `Prelude.hashWithSalt` membershipId
        `Prelude.hashWithSalt` membershipArn
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` createTime
        `Prelude.hashWithSalt` updateTime
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    ConfiguredTableAssociationSummary
  where
  rnf ConfiguredTableAssociationSummary' {..} =
    Prelude.rnf configuredTableId
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn

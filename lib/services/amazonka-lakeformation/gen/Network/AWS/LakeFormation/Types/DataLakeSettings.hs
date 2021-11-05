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
-- Module      : Amazonka.LakeFormation.Types.DataLakeSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataLakeSettings where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.PrincipalPermissions
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure representing a list of AWS Lake Formation principals
-- designated as data lake administrators and lists of principal permission
-- entries for default create database and default create table
-- permissions.
--
-- /See:/ 'newDataLakeSettings' smart constructor.
data DataLakeSettings = DataLakeSettings'
  { -- | A list of AWS Lake Formation principals. Supported principals are IAM
    -- users or IAM roles.
    dataLakeAdmins :: Prelude.Maybe [DataLakePrincipal],
    -- | A list of the resource-owning account IDs that the caller\'s account can
    -- use to share their user access details (user ARNs). The user ARNs can be
    -- logged in the resource owner\'s AWS CloudTrail log.
    --
    -- You may want to specify this property when you are in a high-trust
    -- boundary, such as the same team or company.
    trustedResourceOwners :: Prelude.Maybe [Prelude.Text],
    -- | A structure representing a list of up to three principal permissions
    -- entries for default create database permissions.
    createDatabaseDefaultPermissions :: Prelude.Maybe [PrincipalPermissions],
    -- | A structure representing a list of up to three principal permissions
    -- entries for default create table permissions.
    createTableDefaultPermissions :: Prelude.Maybe [PrincipalPermissions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakeAdmins', 'dataLakeSettings_dataLakeAdmins' - A list of AWS Lake Formation principals. Supported principals are IAM
-- users or IAM roles.
--
-- 'trustedResourceOwners', 'dataLakeSettings_trustedResourceOwners' - A list of the resource-owning account IDs that the caller\'s account can
-- use to share their user access details (user ARNs). The user ARNs can be
-- logged in the resource owner\'s AWS CloudTrail log.
--
-- You may want to specify this property when you are in a high-trust
-- boundary, such as the same team or company.
--
-- 'createDatabaseDefaultPermissions', 'dataLakeSettings_createDatabaseDefaultPermissions' - A structure representing a list of up to three principal permissions
-- entries for default create database permissions.
--
-- 'createTableDefaultPermissions', 'dataLakeSettings_createTableDefaultPermissions' - A structure representing a list of up to three principal permissions
-- entries for default create table permissions.
newDataLakeSettings ::
  DataLakeSettings
newDataLakeSettings =
  DataLakeSettings'
    { dataLakeAdmins = Prelude.Nothing,
      trustedResourceOwners = Prelude.Nothing,
      createDatabaseDefaultPermissions = Prelude.Nothing,
      createTableDefaultPermissions = Prelude.Nothing
    }

-- | A list of AWS Lake Formation principals. Supported principals are IAM
-- users or IAM roles.
dataLakeSettings_dataLakeAdmins :: Lens.Lens' DataLakeSettings (Prelude.Maybe [DataLakePrincipal])
dataLakeSettings_dataLakeAdmins = Lens.lens (\DataLakeSettings' {dataLakeAdmins} -> dataLakeAdmins) (\s@DataLakeSettings' {} a -> s {dataLakeAdmins = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of the resource-owning account IDs that the caller\'s account can
-- use to share their user access details (user ARNs). The user ARNs can be
-- logged in the resource owner\'s AWS CloudTrail log.
--
-- You may want to specify this property when you are in a high-trust
-- boundary, such as the same team or company.
dataLakeSettings_trustedResourceOwners :: Lens.Lens' DataLakeSettings (Prelude.Maybe [Prelude.Text])
dataLakeSettings_trustedResourceOwners = Lens.lens (\DataLakeSettings' {trustedResourceOwners} -> trustedResourceOwners) (\s@DataLakeSettings' {} a -> s {trustedResourceOwners = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A structure representing a list of up to three principal permissions
-- entries for default create database permissions.
dataLakeSettings_createDatabaseDefaultPermissions :: Lens.Lens' DataLakeSettings (Prelude.Maybe [PrincipalPermissions])
dataLakeSettings_createDatabaseDefaultPermissions = Lens.lens (\DataLakeSettings' {createDatabaseDefaultPermissions} -> createDatabaseDefaultPermissions) (\s@DataLakeSettings' {} a -> s {createDatabaseDefaultPermissions = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

-- | A structure representing a list of up to three principal permissions
-- entries for default create table permissions.
dataLakeSettings_createTableDefaultPermissions :: Lens.Lens' DataLakeSettings (Prelude.Maybe [PrincipalPermissions])
dataLakeSettings_createTableDefaultPermissions = Lens.lens (\DataLakeSettings' {createTableDefaultPermissions} -> createTableDefaultPermissions) (\s@DataLakeSettings' {} a -> s {createTableDefaultPermissions = a} :: DataLakeSettings) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DataLakeSettings where
  parseJSON =
    Core.withObject
      "DataLakeSettings"
      ( \x ->
          DataLakeSettings'
            Prelude.<$> (x Core..:? "DataLakeAdmins" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "TrustedResourceOwners"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "CreateDatabaseDefaultPermissions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "CreateTableDefaultPermissions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataLakeSettings

instance Prelude.NFData DataLakeSettings

instance Core.ToJSON DataLakeSettings where
  toJSON DataLakeSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataLakeAdmins" Core..=)
              Prelude.<$> dataLakeAdmins,
            ("TrustedResourceOwners" Core..=)
              Prelude.<$> trustedResourceOwners,
            ("CreateDatabaseDefaultPermissions" Core..=)
              Prelude.<$> createDatabaseDefaultPermissions,
            ("CreateTableDefaultPermissions" Core..=)
              Prelude.<$> createTableDefaultPermissions
          ]
      )

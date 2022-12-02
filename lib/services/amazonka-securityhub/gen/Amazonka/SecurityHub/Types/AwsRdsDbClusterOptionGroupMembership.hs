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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbClusterOptionGroupMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterOptionGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an option group membership for a DB cluster.
--
-- /See:/ 'newAwsRdsDbClusterOptionGroupMembership' smart constructor.
data AwsRdsDbClusterOptionGroupMembership = AwsRdsDbClusterOptionGroupMembership'
  { -- | The status of the DB cluster option group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster option group.
    dbClusterOptionGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbClusterOptionGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbClusterOptionGroupMembership_status' - The status of the DB cluster option group.
--
-- 'dbClusterOptionGroupName', 'awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName' - The name of the DB cluster option group.
newAwsRdsDbClusterOptionGroupMembership ::
  AwsRdsDbClusterOptionGroupMembership
newAwsRdsDbClusterOptionGroupMembership =
  AwsRdsDbClusterOptionGroupMembership'
    { status =
        Prelude.Nothing,
      dbClusterOptionGroupName =
        Prelude.Nothing
    }

-- | The status of the DB cluster option group.
awsRdsDbClusterOptionGroupMembership_status :: Lens.Lens' AwsRdsDbClusterOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbClusterOptionGroupMembership_status = Lens.lens (\AwsRdsDbClusterOptionGroupMembership' {status} -> status) (\s@AwsRdsDbClusterOptionGroupMembership' {} a -> s {status = a} :: AwsRdsDbClusterOptionGroupMembership)

-- | The name of the DB cluster option group.
awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName :: Lens.Lens' AwsRdsDbClusterOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName = Lens.lens (\AwsRdsDbClusterOptionGroupMembership' {dbClusterOptionGroupName} -> dbClusterOptionGroupName) (\s@AwsRdsDbClusterOptionGroupMembership' {} a -> s {dbClusterOptionGroupName = a} :: AwsRdsDbClusterOptionGroupMembership)

instance
  Data.FromJSON
    AwsRdsDbClusterOptionGroupMembership
  where
  parseJSON =
    Data.withObject
      "AwsRdsDbClusterOptionGroupMembership"
      ( \x ->
          AwsRdsDbClusterOptionGroupMembership'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "DbClusterOptionGroupName")
      )

instance
  Prelude.Hashable
    AwsRdsDbClusterOptionGroupMembership
  where
  hashWithSalt
    _salt
    AwsRdsDbClusterOptionGroupMembership' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` dbClusterOptionGroupName

instance
  Prelude.NFData
    AwsRdsDbClusterOptionGroupMembership
  where
  rnf AwsRdsDbClusterOptionGroupMembership' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf dbClusterOptionGroupName

instance
  Data.ToJSON
    AwsRdsDbClusterOptionGroupMembership
  where
  toJSON AwsRdsDbClusterOptionGroupMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            ("DbClusterOptionGroupName" Data..=)
              Prelude.<$> dbClusterOptionGroupName
          ]
      )

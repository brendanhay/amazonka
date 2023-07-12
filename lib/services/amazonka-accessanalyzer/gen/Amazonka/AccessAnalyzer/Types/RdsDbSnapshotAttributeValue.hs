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
-- Module      : Amazonka.AccessAnalyzer.Types.RdsDbSnapshotAttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.RdsDbSnapshotAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name and values of a manual Amazon RDS DB snapshot attribute. Manual
-- DB snapshot attributes are used to authorize other Amazon Web Services
-- accounts to restore a manual DB snapshot.
--
-- /See:/ 'newRdsDbSnapshotAttributeValue' smart constructor.
data RdsDbSnapshotAttributeValue = RdsDbSnapshotAttributeValue'
  { -- | The Amazon Web Services account IDs that have access to the manual
    -- Amazon RDS DB snapshot. If the value @all@ is specified, then the Amazon
    -- RDS DB snapshot is public and can be copied or restored by all Amazon
    -- Web Services accounts.
    --
    -- -   If the configuration is for an existing Amazon RDS DB snapshot and
    --     you do not specify the @accountIds@ in
    --     @RdsDbSnapshotAttributeValue@, then the access preview uses the
    --     existing shared @accountIds@ for the snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the specify the @accountIds@ in @RdsDbSnapshotAttributeValue@, then
    --     the access preview considers the snapshot without any attributes.
    --
    -- -   To propose deletion of an existing shared @accountIds@, you can
    --     specify an empty list for @accountIds@ in the
    --     @RdsDbSnapshotAttributeValue@.
    accountIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbSnapshotAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'rdsDbSnapshotAttributeValue_accountIds' - The Amazon Web Services account IDs that have access to the manual
-- Amazon RDS DB snapshot. If the value @all@ is specified, then the Amazon
-- RDS DB snapshot is public and can be copied or restored by all Amazon
-- Web Services accounts.
--
-- -   If the configuration is for an existing Amazon RDS DB snapshot and
--     you do not specify the @accountIds@ in
--     @RdsDbSnapshotAttributeValue@, then the access preview uses the
--     existing shared @accountIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @accountIds@ in @RdsDbSnapshotAttributeValue@, then
--     the access preview considers the snapshot without any attributes.
--
-- -   To propose deletion of an existing shared @accountIds@, you can
--     specify an empty list for @accountIds@ in the
--     @RdsDbSnapshotAttributeValue@.
newRdsDbSnapshotAttributeValue ::
  RdsDbSnapshotAttributeValue
newRdsDbSnapshotAttributeValue =
  RdsDbSnapshotAttributeValue'
    { accountIds =
        Prelude.Nothing
    }

-- | The Amazon Web Services account IDs that have access to the manual
-- Amazon RDS DB snapshot. If the value @all@ is specified, then the Amazon
-- RDS DB snapshot is public and can be copied or restored by all Amazon
-- Web Services accounts.
--
-- -   If the configuration is for an existing Amazon RDS DB snapshot and
--     you do not specify the @accountIds@ in
--     @RdsDbSnapshotAttributeValue@, then the access preview uses the
--     existing shared @accountIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @accountIds@ in @RdsDbSnapshotAttributeValue@, then
--     the access preview considers the snapshot without any attributes.
--
-- -   To propose deletion of an existing shared @accountIds@, you can
--     specify an empty list for @accountIds@ in the
--     @RdsDbSnapshotAttributeValue@.
rdsDbSnapshotAttributeValue_accountIds :: Lens.Lens' RdsDbSnapshotAttributeValue (Prelude.Maybe [Prelude.Text])
rdsDbSnapshotAttributeValue_accountIds = Lens.lens (\RdsDbSnapshotAttributeValue' {accountIds} -> accountIds) (\s@RdsDbSnapshotAttributeValue' {} a -> s {accountIds = a} :: RdsDbSnapshotAttributeValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RdsDbSnapshotAttributeValue where
  parseJSON =
    Data.withObject
      "RdsDbSnapshotAttributeValue"
      ( \x ->
          RdsDbSnapshotAttributeValue'
            Prelude.<$> (x Data..:? "accountIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RdsDbSnapshotAttributeValue where
  hashWithSalt _salt RdsDbSnapshotAttributeValue' {..} =
    _salt `Prelude.hashWithSalt` accountIds

instance Prelude.NFData RdsDbSnapshotAttributeValue where
  rnf RdsDbSnapshotAttributeValue' {..} =
    Prelude.rnf accountIds

instance Data.ToJSON RdsDbSnapshotAttributeValue where
  toJSON RdsDbSnapshotAttributeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accountIds" Data..=) Prelude.<$> accountIds]
      )

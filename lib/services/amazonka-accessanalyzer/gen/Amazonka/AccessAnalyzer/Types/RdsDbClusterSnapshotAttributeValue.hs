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
-- Module      : Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotAttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values for a manual Amazon RDS DB cluster snapshot attribute.
--
-- /See:/ 'newRdsDbClusterSnapshotAttributeValue' smart constructor.
data RdsDbClusterSnapshotAttributeValue = RdsDbClusterSnapshotAttributeValue'
  { -- | The Amazon Web Services account IDs that have access to the manual
    -- Amazon RDS DB cluster snapshot. If the value @all@ is specified, then
    -- the Amazon RDS DB cluster snapshot is public and can be copied or
    -- restored by all Amazon Web Services accounts.
    --
    -- -   If the configuration is for an existing Amazon RDS DB cluster
    --     snapshot and you do not specify the @accountIds@ in
    --     @RdsDbClusterSnapshotAttributeValue@, then the access preview uses
    --     the existing shared @accountIds@ for the snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the specify the @accountIds@ in
    --     @RdsDbClusterSnapshotAttributeValue@, then the access preview
    --     considers the snapshot without any attributes.
    --
    -- -   To propose deletion of existing shared @accountIds@, you can specify
    --     an empty list for @accountIds@ in the
    --     @RdsDbClusterSnapshotAttributeValue@.
    accountIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbClusterSnapshotAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'rdsDbClusterSnapshotAttributeValue_accountIds' - The Amazon Web Services account IDs that have access to the manual
-- Amazon RDS DB cluster snapshot. If the value @all@ is specified, then
-- the Amazon RDS DB cluster snapshot is public and can be copied or
-- restored by all Amazon Web Services accounts.
--
-- -   If the configuration is for an existing Amazon RDS DB cluster
--     snapshot and you do not specify the @accountIds@ in
--     @RdsDbClusterSnapshotAttributeValue@, then the access preview uses
--     the existing shared @accountIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @accountIds@ in
--     @RdsDbClusterSnapshotAttributeValue@, then the access preview
--     considers the snapshot without any attributes.
--
-- -   To propose deletion of existing shared @accountIds@, you can specify
--     an empty list for @accountIds@ in the
--     @RdsDbClusterSnapshotAttributeValue@.
newRdsDbClusterSnapshotAttributeValue ::
  RdsDbClusterSnapshotAttributeValue
newRdsDbClusterSnapshotAttributeValue =
  RdsDbClusterSnapshotAttributeValue'
    { accountIds =
        Prelude.Nothing
    }

-- | The Amazon Web Services account IDs that have access to the manual
-- Amazon RDS DB cluster snapshot. If the value @all@ is specified, then
-- the Amazon RDS DB cluster snapshot is public and can be copied or
-- restored by all Amazon Web Services accounts.
--
-- -   If the configuration is for an existing Amazon RDS DB cluster
--     snapshot and you do not specify the @accountIds@ in
--     @RdsDbClusterSnapshotAttributeValue@, then the access preview uses
--     the existing shared @accountIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @accountIds@ in
--     @RdsDbClusterSnapshotAttributeValue@, then the access preview
--     considers the snapshot without any attributes.
--
-- -   To propose deletion of existing shared @accountIds@, you can specify
--     an empty list for @accountIds@ in the
--     @RdsDbClusterSnapshotAttributeValue@.
rdsDbClusterSnapshotAttributeValue_accountIds :: Lens.Lens' RdsDbClusterSnapshotAttributeValue (Prelude.Maybe [Prelude.Text])
rdsDbClusterSnapshotAttributeValue_accountIds = Lens.lens (\RdsDbClusterSnapshotAttributeValue' {accountIds} -> accountIds) (\s@RdsDbClusterSnapshotAttributeValue' {} a -> s {accountIds = a} :: RdsDbClusterSnapshotAttributeValue) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RdsDbClusterSnapshotAttributeValue
  where
  parseJSON =
    Data.withObject
      "RdsDbClusterSnapshotAttributeValue"
      ( \x ->
          RdsDbClusterSnapshotAttributeValue'
            Prelude.<$> (x Data..:? "accountIds" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RdsDbClusterSnapshotAttributeValue
  where
  hashWithSalt
    _salt
    RdsDbClusterSnapshotAttributeValue' {..} =
      _salt `Prelude.hashWithSalt` accountIds

instance
  Prelude.NFData
    RdsDbClusterSnapshotAttributeValue
  where
  rnf RdsDbClusterSnapshotAttributeValue' {..} =
    Prelude.rnf accountIds

instance
  Data.ToJSON
    RdsDbClusterSnapshotAttributeValue
  where
  toJSON RdsDbClusterSnapshotAttributeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accountIds" Data..=) Prelude.<$> accountIds]
      )

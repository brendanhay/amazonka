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
-- Module      : Amazonka.MGN.Types.DescribeSourceServersRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DescribeSourceServersRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.LifeCycleState
import Amazonka.MGN.Types.ReplicationType
import qualified Amazonka.Prelude as Prelude

-- | Request to filter Source Servers list.
--
-- /See:/ 'newDescribeSourceServersRequestFilters' smart constructor.
data DescribeSourceServersRequestFilters = DescribeSourceServersRequestFilters'
  { -- | Request to filter Source Servers list by application IDs.
    applicationIDs :: Prelude.Maybe [Prelude.Text],
    -- | Request to filter Source Servers list by archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Request to filter Source Servers list by life cycle states.
    lifeCycleStates :: Prelude.Maybe [LifeCycleState],
    -- | Request to filter Source Servers list by replication type.
    replicationTypes :: Prelude.Maybe [ReplicationType],
    -- | Request to filter Source Servers list by Source Server ID.
    sourceServerIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceServersRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIDs', 'describeSourceServersRequestFilters_applicationIDs' - Request to filter Source Servers list by application IDs.
--
-- 'isArchived', 'describeSourceServersRequestFilters_isArchived' - Request to filter Source Servers list by archived.
--
-- 'lifeCycleStates', 'describeSourceServersRequestFilters_lifeCycleStates' - Request to filter Source Servers list by life cycle states.
--
-- 'replicationTypes', 'describeSourceServersRequestFilters_replicationTypes' - Request to filter Source Servers list by replication type.
--
-- 'sourceServerIDs', 'describeSourceServersRequestFilters_sourceServerIDs' - Request to filter Source Servers list by Source Server ID.
newDescribeSourceServersRequestFilters ::
  DescribeSourceServersRequestFilters
newDescribeSourceServersRequestFilters =
  DescribeSourceServersRequestFilters'
    { applicationIDs =
        Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lifeCycleStates = Prelude.Nothing,
      replicationTypes = Prelude.Nothing,
      sourceServerIDs = Prelude.Nothing
    }

-- | Request to filter Source Servers list by application IDs.
describeSourceServersRequestFilters_applicationIDs :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceServersRequestFilters_applicationIDs = Lens.lens (\DescribeSourceServersRequestFilters' {applicationIDs} -> applicationIDs) (\s@DescribeSourceServersRequestFilters' {} a -> s {applicationIDs = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | Request to filter Source Servers list by archived.
describeSourceServersRequestFilters_isArchived :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe Prelude.Bool)
describeSourceServersRequestFilters_isArchived = Lens.lens (\DescribeSourceServersRequestFilters' {isArchived} -> isArchived) (\s@DescribeSourceServersRequestFilters' {} a -> s {isArchived = a} :: DescribeSourceServersRequestFilters)

-- | Request to filter Source Servers list by life cycle states.
describeSourceServersRequestFilters_lifeCycleStates :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [LifeCycleState])
describeSourceServersRequestFilters_lifeCycleStates = Lens.lens (\DescribeSourceServersRequestFilters' {lifeCycleStates} -> lifeCycleStates) (\s@DescribeSourceServersRequestFilters' {} a -> s {lifeCycleStates = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | Request to filter Source Servers list by replication type.
describeSourceServersRequestFilters_replicationTypes :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [ReplicationType])
describeSourceServersRequestFilters_replicationTypes = Lens.lens (\DescribeSourceServersRequestFilters' {replicationTypes} -> replicationTypes) (\s@DescribeSourceServersRequestFilters' {} a -> s {replicationTypes = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | Request to filter Source Servers list by Source Server ID.
describeSourceServersRequestFilters_sourceServerIDs :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceServersRequestFilters_sourceServerIDs = Lens.lens (\DescribeSourceServersRequestFilters' {sourceServerIDs} -> sourceServerIDs) (\s@DescribeSourceServersRequestFilters' {} a -> s {sourceServerIDs = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DescribeSourceServersRequestFilters
  where
  hashWithSalt
    _salt
    DescribeSourceServersRequestFilters' {..} =
      _salt
        `Prelude.hashWithSalt` applicationIDs
        `Prelude.hashWithSalt` isArchived
        `Prelude.hashWithSalt` lifeCycleStates
        `Prelude.hashWithSalt` replicationTypes
        `Prelude.hashWithSalt` sourceServerIDs

instance
  Prelude.NFData
    DescribeSourceServersRequestFilters
  where
  rnf DescribeSourceServersRequestFilters' {..} =
    Prelude.rnf applicationIDs
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf lifeCycleStates
      `Prelude.seq` Prelude.rnf replicationTypes
      `Prelude.seq` Prelude.rnf sourceServerIDs

instance
  Data.ToJSON
    DescribeSourceServersRequestFilters
  where
  toJSON DescribeSourceServersRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationIDs" Data..=)
              Prelude.<$> applicationIDs,
            ("isArchived" Data..=) Prelude.<$> isArchived,
            ("lifeCycleStates" Data..=)
              Prelude.<$> lifeCycleStates,
            ("replicationTypes" Data..=)
              Prelude.<$> replicationTypes,
            ("sourceServerIDs" Data..=)
              Prelude.<$> sourceServerIDs
          ]
      )

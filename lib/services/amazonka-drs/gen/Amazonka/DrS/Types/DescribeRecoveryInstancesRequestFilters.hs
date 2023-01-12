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
-- Module      : Amazonka.DrS.Types.DescribeRecoveryInstancesRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DescribeRecoveryInstancesRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of filters by which to return Recovery Instances.
--
-- /See:/ 'newDescribeRecoveryInstancesRequestFilters' smart constructor.
data DescribeRecoveryInstancesRequestFilters = DescribeRecoveryInstancesRequestFilters'
  { -- | An array of Recovery Instance IDs that should be returned. An empty
    -- array means all Recovery Instances.
    recoveryInstanceIDs :: Prelude.Maybe [Prelude.Text],
    -- | An array of Source Server IDs for which associated Recovery Instances
    -- should be returned.
    sourceServerIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoveryInstancesRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceIDs', 'describeRecoveryInstancesRequestFilters_recoveryInstanceIDs' - An array of Recovery Instance IDs that should be returned. An empty
-- array means all Recovery Instances.
--
-- 'sourceServerIDs', 'describeRecoveryInstancesRequestFilters_sourceServerIDs' - An array of Source Server IDs for which associated Recovery Instances
-- should be returned.
newDescribeRecoveryInstancesRequestFilters ::
  DescribeRecoveryInstancesRequestFilters
newDescribeRecoveryInstancesRequestFilters =
  DescribeRecoveryInstancesRequestFilters'
    { recoveryInstanceIDs =
        Prelude.Nothing,
      sourceServerIDs = Prelude.Nothing
    }

-- | An array of Recovery Instance IDs that should be returned. An empty
-- array means all Recovery Instances.
describeRecoveryInstancesRequestFilters_recoveryInstanceIDs :: Lens.Lens' DescribeRecoveryInstancesRequestFilters (Prelude.Maybe [Prelude.Text])
describeRecoveryInstancesRequestFilters_recoveryInstanceIDs = Lens.lens (\DescribeRecoveryInstancesRequestFilters' {recoveryInstanceIDs} -> recoveryInstanceIDs) (\s@DescribeRecoveryInstancesRequestFilters' {} a -> s {recoveryInstanceIDs = a} :: DescribeRecoveryInstancesRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | An array of Source Server IDs for which associated Recovery Instances
-- should be returned.
describeRecoveryInstancesRequestFilters_sourceServerIDs :: Lens.Lens' DescribeRecoveryInstancesRequestFilters (Prelude.Maybe [Prelude.Text])
describeRecoveryInstancesRequestFilters_sourceServerIDs = Lens.lens (\DescribeRecoveryInstancesRequestFilters' {sourceServerIDs} -> sourceServerIDs) (\s@DescribeRecoveryInstancesRequestFilters' {} a -> s {sourceServerIDs = a} :: DescribeRecoveryInstancesRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DescribeRecoveryInstancesRequestFilters
  where
  hashWithSalt
    _salt
    DescribeRecoveryInstancesRequestFilters' {..} =
      _salt `Prelude.hashWithSalt` recoveryInstanceIDs
        `Prelude.hashWithSalt` sourceServerIDs

instance
  Prelude.NFData
    DescribeRecoveryInstancesRequestFilters
  where
  rnf DescribeRecoveryInstancesRequestFilters' {..} =
    Prelude.rnf recoveryInstanceIDs
      `Prelude.seq` Prelude.rnf sourceServerIDs

instance
  Data.ToJSON
    DescribeRecoveryInstancesRequestFilters
  where
  toJSON DescribeRecoveryInstancesRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recoveryInstanceIDs" Data..=)
              Prelude.<$> recoveryInstanceIDs,
            ("sourceServerIDs" Data..=)
              Prelude.<$> sourceServerIDs
          ]
      )

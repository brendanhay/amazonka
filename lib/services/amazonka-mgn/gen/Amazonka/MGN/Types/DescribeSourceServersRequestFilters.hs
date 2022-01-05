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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DescribeSourceServersRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Request to filter Source Servers list.
--
-- /See:/ 'newDescribeSourceServersRequestFilters' smart constructor.
data DescribeSourceServersRequestFilters = DescribeSourceServersRequestFilters'
  { -- | Request to filter Source Servers list by Source Server ID.
    sourceServerIDs :: Prelude.Maybe [Prelude.Text],
    -- | Request to filter Source Servers list by archived.
    isArchived :: Prelude.Maybe Prelude.Bool
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
-- 'sourceServerIDs', 'describeSourceServersRequestFilters_sourceServerIDs' - Request to filter Source Servers list by Source Server ID.
--
-- 'isArchived', 'describeSourceServersRequestFilters_isArchived' - Request to filter Source Servers list by archived.
newDescribeSourceServersRequestFilters ::
  DescribeSourceServersRequestFilters
newDescribeSourceServersRequestFilters =
  DescribeSourceServersRequestFilters'
    { sourceServerIDs =
        Prelude.Nothing,
      isArchived = Prelude.Nothing
    }

-- | Request to filter Source Servers list by Source Server ID.
describeSourceServersRequestFilters_sourceServerIDs :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceServersRequestFilters_sourceServerIDs = Lens.lens (\DescribeSourceServersRequestFilters' {sourceServerIDs} -> sourceServerIDs) (\s@DescribeSourceServersRequestFilters' {} a -> s {sourceServerIDs = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | Request to filter Source Servers list by archived.
describeSourceServersRequestFilters_isArchived :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe Prelude.Bool)
describeSourceServersRequestFilters_isArchived = Lens.lens (\DescribeSourceServersRequestFilters' {isArchived} -> isArchived) (\s@DescribeSourceServersRequestFilters' {} a -> s {isArchived = a} :: DescribeSourceServersRequestFilters)

instance
  Prelude.Hashable
    DescribeSourceServersRequestFilters
  where
  hashWithSalt
    _salt
    DescribeSourceServersRequestFilters' {..} =
      _salt `Prelude.hashWithSalt` sourceServerIDs
        `Prelude.hashWithSalt` isArchived

instance
  Prelude.NFData
    DescribeSourceServersRequestFilters
  where
  rnf DescribeSourceServersRequestFilters' {..} =
    Prelude.rnf sourceServerIDs
      `Prelude.seq` Prelude.rnf isArchived

instance
  Core.ToJSON
    DescribeSourceServersRequestFilters
  where
  toJSON DescribeSourceServersRequestFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sourceServerIDs" Core..=)
              Prelude.<$> sourceServerIDs,
            ("isArchived" Core..=) Prelude.<$> isArchived
          ]
      )

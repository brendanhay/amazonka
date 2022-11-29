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
-- Module      : Amazonka.DrS.Types.DescribeSourceServersRequestFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DescribeSourceServersRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of filters by which to return Source Servers.
--
-- /See:/ 'newDescribeSourceServersRequestFilters' smart constructor.
data DescribeSourceServersRequestFilters = DescribeSourceServersRequestFilters'
  { -- | An ID that describes the hardware of the Source Server. This is either
    -- an EC2 instance id, a VMware uuid or a mac address.
    hardwareId :: Prelude.Maybe Prelude.Text,
    -- | An array of Source Servers IDs that should be returned. An empty array
    -- means all Source Servers.
    sourceServerIDs :: Prelude.Maybe [Prelude.Text],
    -- | An array of staging account IDs that extended source servers belong to.
    -- An empty array means all source servers will be shown.
    stagingAccountIDs :: Prelude.Maybe [Prelude.Text]
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
-- 'hardwareId', 'describeSourceServersRequestFilters_hardwareId' - An ID that describes the hardware of the Source Server. This is either
-- an EC2 instance id, a VMware uuid or a mac address.
--
-- 'sourceServerIDs', 'describeSourceServersRequestFilters_sourceServerIDs' - An array of Source Servers IDs that should be returned. An empty array
-- means all Source Servers.
--
-- 'stagingAccountIDs', 'describeSourceServersRequestFilters_stagingAccountIDs' - An array of staging account IDs that extended source servers belong to.
-- An empty array means all source servers will be shown.
newDescribeSourceServersRequestFilters ::
  DescribeSourceServersRequestFilters
newDescribeSourceServersRequestFilters =
  DescribeSourceServersRequestFilters'
    { hardwareId =
        Prelude.Nothing,
      sourceServerIDs = Prelude.Nothing,
      stagingAccountIDs = Prelude.Nothing
    }

-- | An ID that describes the hardware of the Source Server. This is either
-- an EC2 instance id, a VMware uuid or a mac address.
describeSourceServersRequestFilters_hardwareId :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe Prelude.Text)
describeSourceServersRequestFilters_hardwareId = Lens.lens (\DescribeSourceServersRequestFilters' {hardwareId} -> hardwareId) (\s@DescribeSourceServersRequestFilters' {} a -> s {hardwareId = a} :: DescribeSourceServersRequestFilters)

-- | An array of Source Servers IDs that should be returned. An empty array
-- means all Source Servers.
describeSourceServersRequestFilters_sourceServerIDs :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceServersRequestFilters_sourceServerIDs = Lens.lens (\DescribeSourceServersRequestFilters' {sourceServerIDs} -> sourceServerIDs) (\s@DescribeSourceServersRequestFilters' {} a -> s {sourceServerIDs = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | An array of staging account IDs that extended source servers belong to.
-- An empty array means all source servers will be shown.
describeSourceServersRequestFilters_stagingAccountIDs :: Lens.Lens' DescribeSourceServersRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceServersRequestFilters_stagingAccountIDs = Lens.lens (\DescribeSourceServersRequestFilters' {stagingAccountIDs} -> stagingAccountIDs) (\s@DescribeSourceServersRequestFilters' {} a -> s {stagingAccountIDs = a} :: DescribeSourceServersRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DescribeSourceServersRequestFilters
  where
  hashWithSalt
    _salt
    DescribeSourceServersRequestFilters' {..} =
      _salt `Prelude.hashWithSalt` hardwareId
        `Prelude.hashWithSalt` sourceServerIDs
        `Prelude.hashWithSalt` stagingAccountIDs

instance
  Prelude.NFData
    DescribeSourceServersRequestFilters
  where
  rnf DescribeSourceServersRequestFilters' {..} =
    Prelude.rnf hardwareId
      `Prelude.seq` Prelude.rnf sourceServerIDs
      `Prelude.seq` Prelude.rnf stagingAccountIDs

instance
  Core.ToJSON
    DescribeSourceServersRequestFilters
  where
  toJSON DescribeSourceServersRequestFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hardwareId" Core..=) Prelude.<$> hardwareId,
            ("sourceServerIDs" Core..=)
              Prelude.<$> sourceServerIDs,
            ("stagingAccountIDs" Core..=)
              Prelude.<$> stagingAccountIDs
          ]
      )

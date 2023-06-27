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
-- Module      : Amazonka.DrS.Types.DescribeSourceNetworksRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DescribeSourceNetworksRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of filters by which to return Source Networks.
--
-- /See:/ 'newDescribeSourceNetworksRequestFilters' smart constructor.
data DescribeSourceNetworksRequestFilters = DescribeSourceNetworksRequestFilters'
  { -- | Filter Source Networks by account ID containing the protected VPCs.
    originAccountID :: Prelude.Maybe Prelude.Text,
    -- | Filter Source Networks by the region containing the protected VPCs.
    originRegion :: Prelude.Maybe Prelude.Text,
    -- | An array of Source Network IDs that should be returned. An empty array
    -- means all Source Networks.
    sourceNetworkIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceNetworksRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccountID', 'describeSourceNetworksRequestFilters_originAccountID' - Filter Source Networks by account ID containing the protected VPCs.
--
-- 'originRegion', 'describeSourceNetworksRequestFilters_originRegion' - Filter Source Networks by the region containing the protected VPCs.
--
-- 'sourceNetworkIDs', 'describeSourceNetworksRequestFilters_sourceNetworkIDs' - An array of Source Network IDs that should be returned. An empty array
-- means all Source Networks.
newDescribeSourceNetworksRequestFilters ::
  DescribeSourceNetworksRequestFilters
newDescribeSourceNetworksRequestFilters =
  DescribeSourceNetworksRequestFilters'
    { originAccountID =
        Prelude.Nothing,
      originRegion = Prelude.Nothing,
      sourceNetworkIDs = Prelude.Nothing
    }

-- | Filter Source Networks by account ID containing the protected VPCs.
describeSourceNetworksRequestFilters_originAccountID :: Lens.Lens' DescribeSourceNetworksRequestFilters (Prelude.Maybe Prelude.Text)
describeSourceNetworksRequestFilters_originAccountID = Lens.lens (\DescribeSourceNetworksRequestFilters' {originAccountID} -> originAccountID) (\s@DescribeSourceNetworksRequestFilters' {} a -> s {originAccountID = a} :: DescribeSourceNetworksRequestFilters)

-- | Filter Source Networks by the region containing the protected VPCs.
describeSourceNetworksRequestFilters_originRegion :: Lens.Lens' DescribeSourceNetworksRequestFilters (Prelude.Maybe Prelude.Text)
describeSourceNetworksRequestFilters_originRegion = Lens.lens (\DescribeSourceNetworksRequestFilters' {originRegion} -> originRegion) (\s@DescribeSourceNetworksRequestFilters' {} a -> s {originRegion = a} :: DescribeSourceNetworksRequestFilters)

-- | An array of Source Network IDs that should be returned. An empty array
-- means all Source Networks.
describeSourceNetworksRequestFilters_sourceNetworkIDs :: Lens.Lens' DescribeSourceNetworksRequestFilters (Prelude.Maybe [Prelude.Text])
describeSourceNetworksRequestFilters_sourceNetworkIDs = Lens.lens (\DescribeSourceNetworksRequestFilters' {sourceNetworkIDs} -> sourceNetworkIDs) (\s@DescribeSourceNetworksRequestFilters' {} a -> s {sourceNetworkIDs = a} :: DescribeSourceNetworksRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DescribeSourceNetworksRequestFilters
  where
  hashWithSalt
    _salt
    DescribeSourceNetworksRequestFilters' {..} =
      _salt
        `Prelude.hashWithSalt` originAccountID
        `Prelude.hashWithSalt` originRegion
        `Prelude.hashWithSalt` sourceNetworkIDs

instance
  Prelude.NFData
    DescribeSourceNetworksRequestFilters
  where
  rnf DescribeSourceNetworksRequestFilters' {..} =
    Prelude.rnf originAccountID
      `Prelude.seq` Prelude.rnf originRegion
      `Prelude.seq` Prelude.rnf sourceNetworkIDs

instance
  Data.ToJSON
    DescribeSourceNetworksRequestFilters
  where
  toJSON DescribeSourceNetworksRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("originAccountID" Data..=)
              Prelude.<$> originAccountID,
            ("originRegion" Data..=) Prelude.<$> originRegion,
            ("sourceNetworkIDs" Data..=)
              Prelude.<$> sourceNetworkIDs
          ]
      )

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
-- Module      : Amazonka.OpenSearch.Types.ConnectionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ConnectionProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.CrossClusterSearchConnectionProperties
import qualified Amazonka.Prelude as Prelude

-- | The connection properties of an outbound connection.
--
-- /See:/ 'newConnectionProperties' smart constructor.
data ConnectionProperties = ConnectionProperties'
  { -- | The connection properties for cross cluster search.
    crossClusterSearch :: Prelude.Maybe CrossClusterSearchConnectionProperties,
    -- | The Endpoint attribute cannot be modified.
    --
    -- The endpoint of the remote domain. Applicable for VPC_ENDPOINT
    -- connection mode.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearch', 'connectionProperties_crossClusterSearch' - The connection properties for cross cluster search.
--
-- 'endpoint', 'connectionProperties_endpoint' - The Endpoint attribute cannot be modified.
--
-- The endpoint of the remote domain. Applicable for VPC_ENDPOINT
-- connection mode.
newConnectionProperties ::
  ConnectionProperties
newConnectionProperties =
  ConnectionProperties'
    { crossClusterSearch =
        Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The connection properties for cross cluster search.
connectionProperties_crossClusterSearch :: Lens.Lens' ConnectionProperties (Prelude.Maybe CrossClusterSearchConnectionProperties)
connectionProperties_crossClusterSearch = Lens.lens (\ConnectionProperties' {crossClusterSearch} -> crossClusterSearch) (\s@ConnectionProperties' {} a -> s {crossClusterSearch = a} :: ConnectionProperties)

-- | The Endpoint attribute cannot be modified.
--
-- The endpoint of the remote domain. Applicable for VPC_ENDPOINT
-- connection mode.
connectionProperties_endpoint :: Lens.Lens' ConnectionProperties (Prelude.Maybe Prelude.Text)
connectionProperties_endpoint = Lens.lens (\ConnectionProperties' {endpoint} -> endpoint) (\s@ConnectionProperties' {} a -> s {endpoint = a} :: ConnectionProperties)

instance Data.FromJSON ConnectionProperties where
  parseJSON =
    Data.withObject
      "ConnectionProperties"
      ( \x ->
          ConnectionProperties'
            Prelude.<$> (x Data..:? "CrossClusterSearch")
            Prelude.<*> (x Data..:? "Endpoint")
      )

instance Prelude.Hashable ConnectionProperties where
  hashWithSalt _salt ConnectionProperties' {..} =
    _salt
      `Prelude.hashWithSalt` crossClusterSearch
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData ConnectionProperties where
  rnf ConnectionProperties' {..} =
    Prelude.rnf crossClusterSearch
      `Prelude.seq` Prelude.rnf endpoint

instance Data.ToJSON ConnectionProperties where
  toJSON ConnectionProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrossClusterSearch" Data..=)
              Prelude.<$> crossClusterSearch,
            ("Endpoint" Data..=) Prelude.<$> endpoint
          ]
      )

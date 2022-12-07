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
-- Module      : Amazonka.EC2.Types.AccessScopePathRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AccessScopePathRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PathStatementRequest
import Amazonka.EC2.Types.ThroughResourcesStatementRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a path.
--
-- /See:/ 'newAccessScopePathRequest' smart constructor.
data AccessScopePathRequest = AccessScopePathRequest'
  { -- | The destination.
    destination :: Prelude.Maybe PathStatementRequest,
    -- | The source.
    source :: Prelude.Maybe PathStatementRequest,
    -- | The through resources.
    throughResources :: Prelude.Maybe [ThroughResourcesStatementRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessScopePathRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'accessScopePathRequest_destination' - The destination.
--
-- 'source', 'accessScopePathRequest_source' - The source.
--
-- 'throughResources', 'accessScopePathRequest_throughResources' - The through resources.
newAccessScopePathRequest ::
  AccessScopePathRequest
newAccessScopePathRequest =
  AccessScopePathRequest'
    { destination =
        Prelude.Nothing,
      source = Prelude.Nothing,
      throughResources = Prelude.Nothing
    }

-- | The destination.
accessScopePathRequest_destination :: Lens.Lens' AccessScopePathRequest (Prelude.Maybe PathStatementRequest)
accessScopePathRequest_destination = Lens.lens (\AccessScopePathRequest' {destination} -> destination) (\s@AccessScopePathRequest' {} a -> s {destination = a} :: AccessScopePathRequest)

-- | The source.
accessScopePathRequest_source :: Lens.Lens' AccessScopePathRequest (Prelude.Maybe PathStatementRequest)
accessScopePathRequest_source = Lens.lens (\AccessScopePathRequest' {source} -> source) (\s@AccessScopePathRequest' {} a -> s {source = a} :: AccessScopePathRequest)

-- | The through resources.
accessScopePathRequest_throughResources :: Lens.Lens' AccessScopePathRequest (Prelude.Maybe [ThroughResourcesStatementRequest])
accessScopePathRequest_throughResources = Lens.lens (\AccessScopePathRequest' {throughResources} -> throughResources) (\s@AccessScopePathRequest' {} a -> s {throughResources = a} :: AccessScopePathRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AccessScopePathRequest where
  hashWithSalt _salt AccessScopePathRequest' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` throughResources

instance Prelude.NFData AccessScopePathRequest where
  rnf AccessScopePathRequest' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf throughResources

instance Data.ToQuery AccessScopePathRequest where
  toQuery AccessScopePathRequest' {..} =
    Prelude.mconcat
      [ "Destination" Data.=: destination,
        "Source" Data.=: source,
        Data.toQuery
          ( Data.toQueryList "ThroughResource"
              Prelude.<$> throughResources
          )
      ]

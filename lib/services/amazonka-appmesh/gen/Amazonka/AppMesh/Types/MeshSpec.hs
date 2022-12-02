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
-- Module      : Amazonka.AppMesh.Types.MeshSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.MeshSpec where

import Amazonka.AppMesh.Types.EgressFilter
import Amazonka.AppMesh.Types.MeshServiceDiscovery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the specification of a service mesh.
--
-- /See:/ 'newMeshSpec' smart constructor.
data MeshSpec = MeshSpec'
  { -- | The egress filter rules for the service mesh.
    egressFilter :: Prelude.Maybe EgressFilter,
    serviceDiscovery :: Prelude.Maybe MeshServiceDiscovery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeshSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressFilter', 'meshSpec_egressFilter' - The egress filter rules for the service mesh.
--
-- 'serviceDiscovery', 'meshSpec_serviceDiscovery' - Undocumented member.
newMeshSpec ::
  MeshSpec
newMeshSpec =
  MeshSpec'
    { egressFilter = Prelude.Nothing,
      serviceDiscovery = Prelude.Nothing
    }

-- | The egress filter rules for the service mesh.
meshSpec_egressFilter :: Lens.Lens' MeshSpec (Prelude.Maybe EgressFilter)
meshSpec_egressFilter = Lens.lens (\MeshSpec' {egressFilter} -> egressFilter) (\s@MeshSpec' {} a -> s {egressFilter = a} :: MeshSpec)

-- | Undocumented member.
meshSpec_serviceDiscovery :: Lens.Lens' MeshSpec (Prelude.Maybe MeshServiceDiscovery)
meshSpec_serviceDiscovery = Lens.lens (\MeshSpec' {serviceDiscovery} -> serviceDiscovery) (\s@MeshSpec' {} a -> s {serviceDiscovery = a} :: MeshSpec)

instance Data.FromJSON MeshSpec where
  parseJSON =
    Data.withObject
      "MeshSpec"
      ( \x ->
          MeshSpec'
            Prelude.<$> (x Data..:? "egressFilter")
            Prelude.<*> (x Data..:? "serviceDiscovery")
      )

instance Prelude.Hashable MeshSpec where
  hashWithSalt _salt MeshSpec' {..} =
    _salt `Prelude.hashWithSalt` egressFilter
      `Prelude.hashWithSalt` serviceDiscovery

instance Prelude.NFData MeshSpec where
  rnf MeshSpec' {..} =
    Prelude.rnf egressFilter
      `Prelude.seq` Prelude.rnf serviceDiscovery

instance Data.ToJSON MeshSpec where
  toJSON MeshSpec' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("egressFilter" Data..=) Prelude.<$> egressFilter,
            ("serviceDiscovery" Data..=)
              Prelude.<$> serviceDiscovery
          ]
      )

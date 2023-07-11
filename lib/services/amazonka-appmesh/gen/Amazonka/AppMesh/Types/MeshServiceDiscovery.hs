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
-- Module      : Amazonka.AppMesh.Types.MeshServiceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.MeshServiceDiscovery where

import Amazonka.AppMesh.Types.IpPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the service discovery information for a
-- service mesh.
--
-- /See:/ 'newMeshServiceDiscovery' smart constructor.
data MeshServiceDiscovery = MeshServiceDiscovery'
  { -- | The IP version to use to control traffic within the mesh.
    ipPreference :: Prelude.Maybe IpPreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeshServiceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipPreference', 'meshServiceDiscovery_ipPreference' - The IP version to use to control traffic within the mesh.
newMeshServiceDiscovery ::
  MeshServiceDiscovery
newMeshServiceDiscovery =
  MeshServiceDiscovery'
    { ipPreference =
        Prelude.Nothing
    }

-- | The IP version to use to control traffic within the mesh.
meshServiceDiscovery_ipPreference :: Lens.Lens' MeshServiceDiscovery (Prelude.Maybe IpPreference)
meshServiceDiscovery_ipPreference = Lens.lens (\MeshServiceDiscovery' {ipPreference} -> ipPreference) (\s@MeshServiceDiscovery' {} a -> s {ipPreference = a} :: MeshServiceDiscovery)

instance Data.FromJSON MeshServiceDiscovery where
  parseJSON =
    Data.withObject
      "MeshServiceDiscovery"
      ( \x ->
          MeshServiceDiscovery'
            Prelude.<$> (x Data..:? "ipPreference")
      )

instance Prelude.Hashable MeshServiceDiscovery where
  hashWithSalt _salt MeshServiceDiscovery' {..} =
    _salt `Prelude.hashWithSalt` ipPreference

instance Prelude.NFData MeshServiceDiscovery where
  rnf MeshServiceDiscovery' {..} =
    Prelude.rnf ipPreference

instance Data.ToJSON MeshServiceDiscovery where
  toJSON MeshServiceDiscovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ipPreference" Data..=) Prelude.<$> ipPreference]
      )

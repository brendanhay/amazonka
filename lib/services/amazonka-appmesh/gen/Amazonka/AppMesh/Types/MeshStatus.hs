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
-- Module      : Amazonka.AppMesh.Types.MeshStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.MeshStatus where

import Amazonka.AppMesh.Types.MeshStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the status of a service mesh.
--
-- /See:/ 'newMeshStatus' smart constructor.
data MeshStatus = MeshStatus'
  { -- | The current mesh status.
    status :: Prelude.Maybe MeshStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeshStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'meshStatus_status' - The current mesh status.
newMeshStatus ::
  MeshStatus
newMeshStatus = MeshStatus' {status = Prelude.Nothing}

-- | The current mesh status.
meshStatus_status :: Lens.Lens' MeshStatus (Prelude.Maybe MeshStatusCode)
meshStatus_status = Lens.lens (\MeshStatus' {status} -> status) (\s@MeshStatus' {} a -> s {status = a} :: MeshStatus)

instance Data.FromJSON MeshStatus where
  parseJSON =
    Data.withObject
      "MeshStatus"
      ( \x ->
          MeshStatus' Prelude.<$> (x Data..:? "status")
      )

instance Prelude.Hashable MeshStatus where
  hashWithSalt _salt MeshStatus' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData MeshStatus where
  rnf MeshStatus' {..} = Prelude.rnf status

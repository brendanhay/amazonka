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
-- Module      : Network.AWS.AppMesh.Types.MeshSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.MeshSpec where

import Network.AWS.AppMesh.Types.EgressFilter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the specification of a service mesh.
--
-- /See:/ 'newMeshSpec' smart constructor.
data MeshSpec = MeshSpec'
  { -- | The egress filter rules for the service mesh.
    egressFilter :: Prelude.Maybe EgressFilter
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
newMeshSpec ::
  MeshSpec
newMeshSpec =
  MeshSpec' {egressFilter = Prelude.Nothing}

-- | The egress filter rules for the service mesh.
meshSpec_egressFilter :: Lens.Lens' MeshSpec (Prelude.Maybe EgressFilter)
meshSpec_egressFilter = Lens.lens (\MeshSpec' {egressFilter} -> egressFilter) (\s@MeshSpec' {} a -> s {egressFilter = a} :: MeshSpec)

instance Core.FromJSON MeshSpec where
  parseJSON =
    Core.withObject
      "MeshSpec"
      ( \x ->
          MeshSpec' Prelude.<$> (x Core..:? "egressFilter")
      )

instance Prelude.Hashable MeshSpec

instance Prelude.NFData MeshSpec

instance Core.ToJSON MeshSpec where
  toJSON MeshSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [("egressFilter" Core..=) Prelude.<$> egressFilter]
      )

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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteData where

import Amazonka.AppMesh.Types.GatewayRouteSpec
import Amazonka.AppMesh.Types.GatewayRouteStatus
import Amazonka.AppMesh.Types.ResourceMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gateway route returned by a describe
-- operation.
--
-- /See:/ 'newGatewayRouteData' smart constructor.
data GatewayRouteData = GatewayRouteData'
  { -- | The name of the gateway route.
    gatewayRouteName :: Prelude.Text,
    -- | The name of the service mesh that the resource resides in.
    meshName :: Prelude.Text,
    metadata :: ResourceMetadata,
    -- | The specifications of the gateway route.
    spec :: GatewayRouteSpec,
    -- | The status of the gateway route.
    status :: GatewayRouteStatus,
    -- | The virtual gateway that the gateway route is associated with.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayRouteName', 'gatewayRouteData_gatewayRouteName' - The name of the gateway route.
--
-- 'meshName', 'gatewayRouteData_meshName' - The name of the service mesh that the resource resides in.
--
-- 'metadata', 'gatewayRouteData_metadata' - Undocumented member.
--
-- 'spec', 'gatewayRouteData_spec' - The specifications of the gateway route.
--
-- 'status', 'gatewayRouteData_status' - The status of the gateway route.
--
-- 'virtualGatewayName', 'gatewayRouteData_virtualGatewayName' - The virtual gateway that the gateway route is associated with.
newGatewayRouteData ::
  -- | 'gatewayRouteName'
  Prelude.Text ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'metadata'
  ResourceMetadata ->
  -- | 'spec'
  GatewayRouteSpec ->
  -- | 'status'
  GatewayRouteStatus ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  GatewayRouteData
newGatewayRouteData
  pGatewayRouteName_
  pMeshName_
  pMetadata_
  pSpec_
  pStatus_
  pVirtualGatewayName_ =
    GatewayRouteData'
      { gatewayRouteName =
          pGatewayRouteName_,
        meshName = pMeshName_,
        metadata = pMetadata_,
        spec = pSpec_,
        status = pStatus_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The name of the gateway route.
gatewayRouteData_gatewayRouteName :: Lens.Lens' GatewayRouteData Prelude.Text
gatewayRouteData_gatewayRouteName = Lens.lens (\GatewayRouteData' {gatewayRouteName} -> gatewayRouteName) (\s@GatewayRouteData' {} a -> s {gatewayRouteName = a} :: GatewayRouteData)

-- | The name of the service mesh that the resource resides in.
gatewayRouteData_meshName :: Lens.Lens' GatewayRouteData Prelude.Text
gatewayRouteData_meshName = Lens.lens (\GatewayRouteData' {meshName} -> meshName) (\s@GatewayRouteData' {} a -> s {meshName = a} :: GatewayRouteData)

-- | Undocumented member.
gatewayRouteData_metadata :: Lens.Lens' GatewayRouteData ResourceMetadata
gatewayRouteData_metadata = Lens.lens (\GatewayRouteData' {metadata} -> metadata) (\s@GatewayRouteData' {} a -> s {metadata = a} :: GatewayRouteData)

-- | The specifications of the gateway route.
gatewayRouteData_spec :: Lens.Lens' GatewayRouteData GatewayRouteSpec
gatewayRouteData_spec = Lens.lens (\GatewayRouteData' {spec} -> spec) (\s@GatewayRouteData' {} a -> s {spec = a} :: GatewayRouteData)

-- | The status of the gateway route.
gatewayRouteData_status :: Lens.Lens' GatewayRouteData GatewayRouteStatus
gatewayRouteData_status = Lens.lens (\GatewayRouteData' {status} -> status) (\s@GatewayRouteData' {} a -> s {status = a} :: GatewayRouteData)

-- | The virtual gateway that the gateway route is associated with.
gatewayRouteData_virtualGatewayName :: Lens.Lens' GatewayRouteData Prelude.Text
gatewayRouteData_virtualGatewayName = Lens.lens (\GatewayRouteData' {virtualGatewayName} -> virtualGatewayName) (\s@GatewayRouteData' {} a -> s {virtualGatewayName = a} :: GatewayRouteData)

instance Core.FromJSON GatewayRouteData where
  parseJSON =
    Core.withObject
      "GatewayRouteData"
      ( \x ->
          GatewayRouteData'
            Prelude.<$> (x Core..: "gatewayRouteName")
            Prelude.<*> (x Core..: "meshName")
            Prelude.<*> (x Core..: "metadata")
            Prelude.<*> (x Core..: "spec")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "virtualGatewayName")
      )

instance Prelude.Hashable GatewayRouteData where
  hashWithSalt _salt GatewayRouteData' {..} =
    _salt `Prelude.hashWithSalt` gatewayRouteName
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData GatewayRouteData where
  rnf GatewayRouteData' {..} =
    Prelude.rnf gatewayRouteName
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf virtualGatewayName

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
-- Module      : Network.AWS.AppMesh.Types.RouteData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.RouteData where

import Network.AWS.AppMesh.Types.ResourceMetadata
import Network.AWS.AppMesh.Types.RouteSpec
import Network.AWS.AppMesh.Types.RouteStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents a route returned by a describe operation.
--
-- /See:/ 'newRouteData' smart constructor.
data RouteData = RouteData'
  { -- | The name of the service mesh that the route resides in.
    meshName :: Prelude.Text,
    -- | The associated metadata for the route.
    metadata :: ResourceMetadata,
    -- | The name of the route.
    routeName :: Prelude.Text,
    -- | The specifications of the route.
    spec :: RouteSpec,
    -- | The status of the route.
    status :: RouteStatus,
    -- | The virtual router that the route is associated with.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshName', 'routeData_meshName' - The name of the service mesh that the route resides in.
--
-- 'metadata', 'routeData_metadata' - The associated metadata for the route.
--
-- 'routeName', 'routeData_routeName' - The name of the route.
--
-- 'spec', 'routeData_spec' - The specifications of the route.
--
-- 'status', 'routeData_status' - The status of the route.
--
-- 'virtualRouterName', 'routeData_virtualRouterName' - The virtual router that the route is associated with.
newRouteData ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'metadata'
  ResourceMetadata ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'spec'
  RouteSpec ->
  -- | 'status'
  RouteStatus ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  RouteData
newRouteData
  pMeshName_
  pMetadata_
  pRouteName_
  pSpec_
  pStatus_
  pVirtualRouterName_ =
    RouteData'
      { meshName = pMeshName_,
        metadata = pMetadata_,
        routeName = pRouteName_,
        spec = pSpec_,
        status = pStatus_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The name of the service mesh that the route resides in.
routeData_meshName :: Lens.Lens' RouteData Prelude.Text
routeData_meshName = Lens.lens (\RouteData' {meshName} -> meshName) (\s@RouteData' {} a -> s {meshName = a} :: RouteData)

-- | The associated metadata for the route.
routeData_metadata :: Lens.Lens' RouteData ResourceMetadata
routeData_metadata = Lens.lens (\RouteData' {metadata} -> metadata) (\s@RouteData' {} a -> s {metadata = a} :: RouteData)

-- | The name of the route.
routeData_routeName :: Lens.Lens' RouteData Prelude.Text
routeData_routeName = Lens.lens (\RouteData' {routeName} -> routeName) (\s@RouteData' {} a -> s {routeName = a} :: RouteData)

-- | The specifications of the route.
routeData_spec :: Lens.Lens' RouteData RouteSpec
routeData_spec = Lens.lens (\RouteData' {spec} -> spec) (\s@RouteData' {} a -> s {spec = a} :: RouteData)

-- | The status of the route.
routeData_status :: Lens.Lens' RouteData RouteStatus
routeData_status = Lens.lens (\RouteData' {status} -> status) (\s@RouteData' {} a -> s {status = a} :: RouteData)

-- | The virtual router that the route is associated with.
routeData_virtualRouterName :: Lens.Lens' RouteData Prelude.Text
routeData_virtualRouterName = Lens.lens (\RouteData' {virtualRouterName} -> virtualRouterName) (\s@RouteData' {} a -> s {virtualRouterName = a} :: RouteData)

instance Core.FromJSON RouteData where
  parseJSON =
    Core.withObject
      "RouteData"
      ( \x ->
          RouteData'
            Prelude.<$> (x Core..: "meshName")
            Prelude.<*> (x Core..: "metadata")
            Prelude.<*> (x Core..: "routeName")
            Prelude.<*> (x Core..: "spec")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "virtualRouterName")
      )

instance Prelude.Hashable RouteData

instance Prelude.NFData RouteData

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
-- Module      : Amazonka.AppMesh.Types.RouteRef
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.RouteRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a route returned by a list operation.
--
-- /See:/ 'newRouteRef' smart constructor.
data RouteRef = RouteRef'
  { -- | The full Amazon Resource Name (ARN) for the route.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Core.POSIX,
    -- | The name of the service mesh that the route resides in.
    meshName :: Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the resource owner. If the
    -- account ID is not your own, then it\'s the ID of the mesh owner or of
    -- another account that the mesh is shared with. For more information about
    -- mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    resourceOwner :: Prelude.Text,
    -- | The name of the route.
    routeName :: Prelude.Text,
    -- | The version of the resource. Resources are created at version 1, and
    -- this version is incremented each time that they\'re updated.
    version :: Prelude.Integer,
    -- | The virtual router that the route is associated with.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'routeRef_arn' - The full Amazon Resource Name (ARN) for the route.
--
-- 'createdAt', 'routeRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'routeRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'routeRef_meshName' - The name of the service mesh that the route resides in.
--
-- 'meshOwner', 'routeRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'routeRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'routeName', 'routeRef_routeName' - The name of the route.
--
-- 'version', 'routeRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualRouterName', 'routeRef_virtualRouterName' - The virtual router that the route is associated with.
newRouteRef ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'meshOwner'
  Prelude.Text ->
  -- | 'resourceOwner'
  Prelude.Text ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'version'
  Prelude.Integer ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  RouteRef
newRouteRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pRouteName_
  pVersion_
  pVirtualRouterName_ =
    RouteRef'
      { arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Core._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        routeName = pRouteName_,
        version = pVersion_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The full Amazon Resource Name (ARN) for the route.
routeRef_arn :: Lens.Lens' RouteRef Prelude.Text
routeRef_arn = Lens.lens (\RouteRef' {arn} -> arn) (\s@RouteRef' {} a -> s {arn = a} :: RouteRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
routeRef_createdAt :: Lens.Lens' RouteRef Prelude.UTCTime
routeRef_createdAt = Lens.lens (\RouteRef' {createdAt} -> createdAt) (\s@RouteRef' {} a -> s {createdAt = a} :: RouteRef) Prelude.. Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
routeRef_lastUpdatedAt :: Lens.Lens' RouteRef Prelude.UTCTime
routeRef_lastUpdatedAt = Lens.lens (\RouteRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@RouteRef' {} a -> s {lastUpdatedAt = a} :: RouteRef) Prelude.. Core._Time

-- | The name of the service mesh that the route resides in.
routeRef_meshName :: Lens.Lens' RouteRef Prelude.Text
routeRef_meshName = Lens.lens (\RouteRef' {meshName} -> meshName) (\s@RouteRef' {} a -> s {meshName = a} :: RouteRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
routeRef_meshOwner :: Lens.Lens' RouteRef Prelude.Text
routeRef_meshOwner = Lens.lens (\RouteRef' {meshOwner} -> meshOwner) (\s@RouteRef' {} a -> s {meshOwner = a} :: RouteRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
routeRef_resourceOwner :: Lens.Lens' RouteRef Prelude.Text
routeRef_resourceOwner = Lens.lens (\RouteRef' {resourceOwner} -> resourceOwner) (\s@RouteRef' {} a -> s {resourceOwner = a} :: RouteRef)

-- | The name of the route.
routeRef_routeName :: Lens.Lens' RouteRef Prelude.Text
routeRef_routeName = Lens.lens (\RouteRef' {routeName} -> routeName) (\s@RouteRef' {} a -> s {routeName = a} :: RouteRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
routeRef_version :: Lens.Lens' RouteRef Prelude.Integer
routeRef_version = Lens.lens (\RouteRef' {version} -> version) (\s@RouteRef' {} a -> s {version = a} :: RouteRef)

-- | The virtual router that the route is associated with.
routeRef_virtualRouterName :: Lens.Lens' RouteRef Prelude.Text
routeRef_virtualRouterName = Lens.lens (\RouteRef' {virtualRouterName} -> virtualRouterName) (\s@RouteRef' {} a -> s {virtualRouterName = a} :: RouteRef)

instance Core.FromJSON RouteRef where
  parseJSON =
    Core.withObject
      "RouteRef"
      ( \x ->
          RouteRef'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastUpdatedAt")
            Prelude.<*> (x Core..: "meshName")
            Prelude.<*> (x Core..: "meshOwner")
            Prelude.<*> (x Core..: "resourceOwner")
            Prelude.<*> (x Core..: "routeName")
            Prelude.<*> (x Core..: "version")
            Prelude.<*> (x Core..: "virtualRouterName")
      )

instance Prelude.Hashable RouteRef where
  hashWithSalt _salt RouteRef' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` routeName
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData RouteRef where
  rnf RouteRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf routeName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf virtualRouterName

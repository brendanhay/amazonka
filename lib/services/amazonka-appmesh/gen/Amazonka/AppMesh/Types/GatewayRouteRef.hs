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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteRef
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gateway route returned by a list operation.
--
-- /See:/ 'newGatewayRouteRef' smart constructor.
data GatewayRouteRef = GatewayRouteRef'
  { -- | The full Amazon Resource Name (ARN) for the gateway route.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Data.POSIX,
    -- | The name of the gateway route.
    gatewayRouteName :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Data.POSIX,
    -- | The name of the service mesh that the resource resides in.
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
    -- | The version of the resource. Resources are created at version 1, and
    -- this version is incremented each time that they\'re updated.
    version :: Prelude.Integer,
    -- | The virtual gateway that the gateway route is associated with.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'gatewayRouteRef_arn' - The full Amazon Resource Name (ARN) for the gateway route.
--
-- 'createdAt', 'gatewayRouteRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'gatewayRouteName', 'gatewayRouteRef_gatewayRouteName' - The name of the gateway route.
--
-- 'lastUpdatedAt', 'gatewayRouteRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'gatewayRouteRef_meshName' - The name of the service mesh that the resource resides in.
--
-- 'meshOwner', 'gatewayRouteRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'gatewayRouteRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'gatewayRouteRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualGatewayName', 'gatewayRouteRef_virtualGatewayName' - The virtual gateway that the gateway route is associated with.
newGatewayRouteRef ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'gatewayRouteName'
  Prelude.Text ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'meshOwner'
  Prelude.Text ->
  -- | 'resourceOwner'
  Prelude.Text ->
  -- | 'version'
  Prelude.Integer ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  GatewayRouteRef
newGatewayRouteRef
  pArn_
  pCreatedAt_
  pGatewayRouteName_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_
  pVirtualGatewayName_ =
    GatewayRouteRef'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        gatewayRouteName = pGatewayRouteName_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The full Amazon Resource Name (ARN) for the gateway route.
gatewayRouteRef_arn :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_arn = Lens.lens (\GatewayRouteRef' {arn} -> arn) (\s@GatewayRouteRef' {} a -> s {arn = a} :: GatewayRouteRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
gatewayRouteRef_createdAt :: Lens.Lens' GatewayRouteRef Prelude.UTCTime
gatewayRouteRef_createdAt = Lens.lens (\GatewayRouteRef' {createdAt} -> createdAt) (\s@GatewayRouteRef' {} a -> s {createdAt = a} :: GatewayRouteRef) Prelude.. Data._Time

-- | The name of the gateway route.
gatewayRouteRef_gatewayRouteName :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_gatewayRouteName = Lens.lens (\GatewayRouteRef' {gatewayRouteName} -> gatewayRouteName) (\s@GatewayRouteRef' {} a -> s {gatewayRouteName = a} :: GatewayRouteRef)

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
gatewayRouteRef_lastUpdatedAt :: Lens.Lens' GatewayRouteRef Prelude.UTCTime
gatewayRouteRef_lastUpdatedAt = Lens.lens (\GatewayRouteRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@GatewayRouteRef' {} a -> s {lastUpdatedAt = a} :: GatewayRouteRef) Prelude.. Data._Time

-- | The name of the service mesh that the resource resides in.
gatewayRouteRef_meshName :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_meshName = Lens.lens (\GatewayRouteRef' {meshName} -> meshName) (\s@GatewayRouteRef' {} a -> s {meshName = a} :: GatewayRouteRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
gatewayRouteRef_meshOwner :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_meshOwner = Lens.lens (\GatewayRouteRef' {meshOwner} -> meshOwner) (\s@GatewayRouteRef' {} a -> s {meshOwner = a} :: GatewayRouteRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
gatewayRouteRef_resourceOwner :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_resourceOwner = Lens.lens (\GatewayRouteRef' {resourceOwner} -> resourceOwner) (\s@GatewayRouteRef' {} a -> s {resourceOwner = a} :: GatewayRouteRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
gatewayRouteRef_version :: Lens.Lens' GatewayRouteRef Prelude.Integer
gatewayRouteRef_version = Lens.lens (\GatewayRouteRef' {version} -> version) (\s@GatewayRouteRef' {} a -> s {version = a} :: GatewayRouteRef)

-- | The virtual gateway that the gateway route is associated with.
gatewayRouteRef_virtualGatewayName :: Lens.Lens' GatewayRouteRef Prelude.Text
gatewayRouteRef_virtualGatewayName = Lens.lens (\GatewayRouteRef' {virtualGatewayName} -> virtualGatewayName) (\s@GatewayRouteRef' {} a -> s {virtualGatewayName = a} :: GatewayRouteRef)

instance Data.FromJSON GatewayRouteRef where
  parseJSON =
    Data.withObject
      "GatewayRouteRef"
      ( \x ->
          GatewayRouteRef'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "gatewayRouteName")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "meshName")
            Prelude.<*> (x Data..: "meshOwner")
            Prelude.<*> (x Data..: "resourceOwner")
            Prelude.<*> (x Data..: "version")
            Prelude.<*> (x Data..: "virtualGatewayName")
      )

instance Prelude.Hashable GatewayRouteRef where
  hashWithSalt _salt GatewayRouteRef' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` gatewayRouteName
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData GatewayRouteRef where
  rnf GatewayRouteRef' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdAt `Prelude.seq`
        Prelude.rnf gatewayRouteName `Prelude.seq`
          Prelude.rnf lastUpdatedAt `Prelude.seq`
            Prelude.rnf meshName `Prelude.seq`
              Prelude.rnf meshOwner `Prelude.seq`
                Prelude.rnf resourceOwner `Prelude.seq`
                  Prelude.rnf version `Prelude.seq`
                    Prelude.rnf virtualGatewayName

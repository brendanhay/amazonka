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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayRef
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual gateway returned by a list
-- operation.
--
-- /See:/ 'newVirtualGatewayRef' smart constructor.
data VirtualGatewayRef = VirtualGatewayRef'
  { -- | The full Amazon Resource Name (ARN) for the resource.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Core.POSIX,
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
    -- | The name of the resource.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'virtualGatewayRef_arn' - The full Amazon Resource Name (ARN) for the resource.
--
-- 'createdAt', 'virtualGatewayRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'virtualGatewayRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'virtualGatewayRef_meshName' - The name of the service mesh that the resource resides in.
--
-- 'meshOwner', 'virtualGatewayRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'virtualGatewayRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'virtualGatewayRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualGatewayName', 'virtualGatewayRef_virtualGatewayName' - The name of the resource.
newVirtualGatewayRef ::
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
  -- | 'version'
  Prelude.Integer ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  VirtualGatewayRef
newVirtualGatewayRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_
  pVirtualGatewayName_ =
    VirtualGatewayRef'
      { arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Core._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The full Amazon Resource Name (ARN) for the resource.
virtualGatewayRef_arn :: Lens.Lens' VirtualGatewayRef Prelude.Text
virtualGatewayRef_arn = Lens.lens (\VirtualGatewayRef' {arn} -> arn) (\s@VirtualGatewayRef' {} a -> s {arn = a} :: VirtualGatewayRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
virtualGatewayRef_createdAt :: Lens.Lens' VirtualGatewayRef Prelude.UTCTime
virtualGatewayRef_createdAt = Lens.lens (\VirtualGatewayRef' {createdAt} -> createdAt) (\s@VirtualGatewayRef' {} a -> s {createdAt = a} :: VirtualGatewayRef) Prelude.. Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
virtualGatewayRef_lastUpdatedAt :: Lens.Lens' VirtualGatewayRef Prelude.UTCTime
virtualGatewayRef_lastUpdatedAt = Lens.lens (\VirtualGatewayRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@VirtualGatewayRef' {} a -> s {lastUpdatedAt = a} :: VirtualGatewayRef) Prelude.. Core._Time

-- | The name of the service mesh that the resource resides in.
virtualGatewayRef_meshName :: Lens.Lens' VirtualGatewayRef Prelude.Text
virtualGatewayRef_meshName = Lens.lens (\VirtualGatewayRef' {meshName} -> meshName) (\s@VirtualGatewayRef' {} a -> s {meshName = a} :: VirtualGatewayRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualGatewayRef_meshOwner :: Lens.Lens' VirtualGatewayRef Prelude.Text
virtualGatewayRef_meshOwner = Lens.lens (\VirtualGatewayRef' {meshOwner} -> meshOwner) (\s@VirtualGatewayRef' {} a -> s {meshOwner = a} :: VirtualGatewayRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualGatewayRef_resourceOwner :: Lens.Lens' VirtualGatewayRef Prelude.Text
virtualGatewayRef_resourceOwner = Lens.lens (\VirtualGatewayRef' {resourceOwner} -> resourceOwner) (\s@VirtualGatewayRef' {} a -> s {resourceOwner = a} :: VirtualGatewayRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
virtualGatewayRef_version :: Lens.Lens' VirtualGatewayRef Prelude.Integer
virtualGatewayRef_version = Lens.lens (\VirtualGatewayRef' {version} -> version) (\s@VirtualGatewayRef' {} a -> s {version = a} :: VirtualGatewayRef)

-- | The name of the resource.
virtualGatewayRef_virtualGatewayName :: Lens.Lens' VirtualGatewayRef Prelude.Text
virtualGatewayRef_virtualGatewayName = Lens.lens (\VirtualGatewayRef' {virtualGatewayName} -> virtualGatewayName) (\s@VirtualGatewayRef' {} a -> s {virtualGatewayName = a} :: VirtualGatewayRef)

instance Core.FromJSON VirtualGatewayRef where
  parseJSON =
    Core.withObject
      "VirtualGatewayRef"
      ( \x ->
          VirtualGatewayRef'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastUpdatedAt")
            Prelude.<*> (x Core..: "meshName")
            Prelude.<*> (x Core..: "meshOwner")
            Prelude.<*> (x Core..: "resourceOwner")
            Prelude.<*> (x Core..: "version")
            Prelude.<*> (x Core..: "virtualGatewayName")
      )

instance Prelude.Hashable VirtualGatewayRef where
  hashWithSalt _salt VirtualGatewayRef' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData VirtualGatewayRef where
  rnf VirtualGatewayRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf virtualGatewayName

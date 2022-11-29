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
-- Module      : Amazonka.AppMesh.Types.VirtualRouterRef
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualRouterRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual router returned by a list operation.
--
-- /See:/ 'newVirtualRouterRef' smart constructor.
data VirtualRouterRef = VirtualRouterRef'
  { -- | The full Amazon Resource Name (ARN) for the virtual router.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Core.POSIX,
    -- | The name of the service mesh that the virtual router resides in.
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
    -- | The name of the virtual router.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualRouterRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'virtualRouterRef_arn' - The full Amazon Resource Name (ARN) for the virtual router.
--
-- 'createdAt', 'virtualRouterRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'virtualRouterRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'virtualRouterRef_meshName' - The name of the service mesh that the virtual router resides in.
--
-- 'meshOwner', 'virtualRouterRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'virtualRouterRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'virtualRouterRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualRouterName', 'virtualRouterRef_virtualRouterName' - The name of the virtual router.
newVirtualRouterRef ::
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
  -- | 'virtualRouterName'
  Prelude.Text ->
  VirtualRouterRef
newVirtualRouterRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_
  pVirtualRouterName_ =
    VirtualRouterRef'
      { arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Core._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The full Amazon Resource Name (ARN) for the virtual router.
virtualRouterRef_arn :: Lens.Lens' VirtualRouterRef Prelude.Text
virtualRouterRef_arn = Lens.lens (\VirtualRouterRef' {arn} -> arn) (\s@VirtualRouterRef' {} a -> s {arn = a} :: VirtualRouterRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
virtualRouterRef_createdAt :: Lens.Lens' VirtualRouterRef Prelude.UTCTime
virtualRouterRef_createdAt = Lens.lens (\VirtualRouterRef' {createdAt} -> createdAt) (\s@VirtualRouterRef' {} a -> s {createdAt = a} :: VirtualRouterRef) Prelude.. Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
virtualRouterRef_lastUpdatedAt :: Lens.Lens' VirtualRouterRef Prelude.UTCTime
virtualRouterRef_lastUpdatedAt = Lens.lens (\VirtualRouterRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@VirtualRouterRef' {} a -> s {lastUpdatedAt = a} :: VirtualRouterRef) Prelude.. Core._Time

-- | The name of the service mesh that the virtual router resides in.
virtualRouterRef_meshName :: Lens.Lens' VirtualRouterRef Prelude.Text
virtualRouterRef_meshName = Lens.lens (\VirtualRouterRef' {meshName} -> meshName) (\s@VirtualRouterRef' {} a -> s {meshName = a} :: VirtualRouterRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualRouterRef_meshOwner :: Lens.Lens' VirtualRouterRef Prelude.Text
virtualRouterRef_meshOwner = Lens.lens (\VirtualRouterRef' {meshOwner} -> meshOwner) (\s@VirtualRouterRef' {} a -> s {meshOwner = a} :: VirtualRouterRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualRouterRef_resourceOwner :: Lens.Lens' VirtualRouterRef Prelude.Text
virtualRouterRef_resourceOwner = Lens.lens (\VirtualRouterRef' {resourceOwner} -> resourceOwner) (\s@VirtualRouterRef' {} a -> s {resourceOwner = a} :: VirtualRouterRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
virtualRouterRef_version :: Lens.Lens' VirtualRouterRef Prelude.Integer
virtualRouterRef_version = Lens.lens (\VirtualRouterRef' {version} -> version) (\s@VirtualRouterRef' {} a -> s {version = a} :: VirtualRouterRef)

-- | The name of the virtual router.
virtualRouterRef_virtualRouterName :: Lens.Lens' VirtualRouterRef Prelude.Text
virtualRouterRef_virtualRouterName = Lens.lens (\VirtualRouterRef' {virtualRouterName} -> virtualRouterName) (\s@VirtualRouterRef' {} a -> s {virtualRouterName = a} :: VirtualRouterRef)

instance Core.FromJSON VirtualRouterRef where
  parseJSON =
    Core.withObject
      "VirtualRouterRef"
      ( \x ->
          VirtualRouterRef'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastUpdatedAt")
            Prelude.<*> (x Core..: "meshName")
            Prelude.<*> (x Core..: "meshOwner")
            Prelude.<*> (x Core..: "resourceOwner")
            Prelude.<*> (x Core..: "version")
            Prelude.<*> (x Core..: "virtualRouterName")
      )

instance Prelude.Hashable VirtualRouterRef where
  hashWithSalt _salt VirtualRouterRef' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData VirtualRouterRef where
  rnf VirtualRouterRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf virtualRouterName

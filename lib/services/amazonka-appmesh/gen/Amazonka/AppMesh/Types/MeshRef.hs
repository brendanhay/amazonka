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
-- Module      : Amazonka.AppMesh.Types.MeshRef
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.MeshRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a service mesh returned by a list operation.
--
-- /See:/ 'newMeshRef' smart constructor.
data MeshRef = MeshRef'
  { -- | The full Amazon Resource Name (ARN) of the service mesh.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Data.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Data.POSIX,
    -- | The name of the service mesh.
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
    version :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeshRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'meshRef_arn' - The full Amazon Resource Name (ARN) of the service mesh.
--
-- 'createdAt', 'meshRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'meshRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'meshRef_meshName' - The name of the service mesh.
--
-- 'meshOwner', 'meshRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'meshRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'meshRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
newMeshRef ::
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
  MeshRef
newMeshRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_ =
    MeshRef'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_
      }

-- | The full Amazon Resource Name (ARN) of the service mesh.
meshRef_arn :: Lens.Lens' MeshRef Prelude.Text
meshRef_arn = Lens.lens (\MeshRef' {arn} -> arn) (\s@MeshRef' {} a -> s {arn = a} :: MeshRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
meshRef_createdAt :: Lens.Lens' MeshRef Prelude.UTCTime
meshRef_createdAt = Lens.lens (\MeshRef' {createdAt} -> createdAt) (\s@MeshRef' {} a -> s {createdAt = a} :: MeshRef) Prelude.. Data._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
meshRef_lastUpdatedAt :: Lens.Lens' MeshRef Prelude.UTCTime
meshRef_lastUpdatedAt = Lens.lens (\MeshRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@MeshRef' {} a -> s {lastUpdatedAt = a} :: MeshRef) Prelude.. Data._Time

-- | The name of the service mesh.
meshRef_meshName :: Lens.Lens' MeshRef Prelude.Text
meshRef_meshName = Lens.lens (\MeshRef' {meshName} -> meshName) (\s@MeshRef' {} a -> s {meshName = a} :: MeshRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
meshRef_meshOwner :: Lens.Lens' MeshRef Prelude.Text
meshRef_meshOwner = Lens.lens (\MeshRef' {meshOwner} -> meshOwner) (\s@MeshRef' {} a -> s {meshOwner = a} :: MeshRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
meshRef_resourceOwner :: Lens.Lens' MeshRef Prelude.Text
meshRef_resourceOwner = Lens.lens (\MeshRef' {resourceOwner} -> resourceOwner) (\s@MeshRef' {} a -> s {resourceOwner = a} :: MeshRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
meshRef_version :: Lens.Lens' MeshRef Prelude.Integer
meshRef_version = Lens.lens (\MeshRef' {version} -> version) (\s@MeshRef' {} a -> s {version = a} :: MeshRef)

instance Data.FromJSON MeshRef where
  parseJSON =
    Data.withObject
      "MeshRef"
      ( \x ->
          MeshRef'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "meshName")
            Prelude.<*> (x Data..: "meshOwner")
            Prelude.<*> (x Data..: "resourceOwner")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable MeshRef where
  hashWithSalt _salt MeshRef' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version

instance Prelude.NFData MeshRef where
  rnf MeshRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf version

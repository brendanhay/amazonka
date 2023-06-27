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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeRef
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual node returned by a list operation.
--
-- /See:/ 'newVirtualNodeRef' smart constructor.
data VirtualNodeRef = VirtualNodeRef'
  { -- | The full Amazon Resource Name (ARN) for the virtual node.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Data.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Data.POSIX,
    -- | The name of the service mesh that the virtual node resides in.
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
    -- | The name of the virtual node.
    virtualNodeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'virtualNodeRef_arn' - The full Amazon Resource Name (ARN) for the virtual node.
--
-- 'createdAt', 'virtualNodeRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'virtualNodeRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'virtualNodeRef_meshName' - The name of the service mesh that the virtual node resides in.
--
-- 'meshOwner', 'virtualNodeRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'virtualNodeRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'virtualNodeRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualNodeName', 'virtualNodeRef_virtualNodeName' - The name of the virtual node.
newVirtualNodeRef ::
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
  -- | 'virtualNodeName'
  Prelude.Text ->
  VirtualNodeRef
newVirtualNodeRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_
  pVirtualNodeName_ =
    VirtualNodeRef'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_,
        virtualNodeName = pVirtualNodeName_
      }

-- | The full Amazon Resource Name (ARN) for the virtual node.
virtualNodeRef_arn :: Lens.Lens' VirtualNodeRef Prelude.Text
virtualNodeRef_arn = Lens.lens (\VirtualNodeRef' {arn} -> arn) (\s@VirtualNodeRef' {} a -> s {arn = a} :: VirtualNodeRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
virtualNodeRef_createdAt :: Lens.Lens' VirtualNodeRef Prelude.UTCTime
virtualNodeRef_createdAt = Lens.lens (\VirtualNodeRef' {createdAt} -> createdAt) (\s@VirtualNodeRef' {} a -> s {createdAt = a} :: VirtualNodeRef) Prelude.. Data._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
virtualNodeRef_lastUpdatedAt :: Lens.Lens' VirtualNodeRef Prelude.UTCTime
virtualNodeRef_lastUpdatedAt = Lens.lens (\VirtualNodeRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@VirtualNodeRef' {} a -> s {lastUpdatedAt = a} :: VirtualNodeRef) Prelude.. Data._Time

-- | The name of the service mesh that the virtual node resides in.
virtualNodeRef_meshName :: Lens.Lens' VirtualNodeRef Prelude.Text
virtualNodeRef_meshName = Lens.lens (\VirtualNodeRef' {meshName} -> meshName) (\s@VirtualNodeRef' {} a -> s {meshName = a} :: VirtualNodeRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualNodeRef_meshOwner :: Lens.Lens' VirtualNodeRef Prelude.Text
virtualNodeRef_meshOwner = Lens.lens (\VirtualNodeRef' {meshOwner} -> meshOwner) (\s@VirtualNodeRef' {} a -> s {meshOwner = a} :: VirtualNodeRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualNodeRef_resourceOwner :: Lens.Lens' VirtualNodeRef Prelude.Text
virtualNodeRef_resourceOwner = Lens.lens (\VirtualNodeRef' {resourceOwner} -> resourceOwner) (\s@VirtualNodeRef' {} a -> s {resourceOwner = a} :: VirtualNodeRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
virtualNodeRef_version :: Lens.Lens' VirtualNodeRef Prelude.Integer
virtualNodeRef_version = Lens.lens (\VirtualNodeRef' {version} -> version) (\s@VirtualNodeRef' {} a -> s {version = a} :: VirtualNodeRef)

-- | The name of the virtual node.
virtualNodeRef_virtualNodeName :: Lens.Lens' VirtualNodeRef Prelude.Text
virtualNodeRef_virtualNodeName = Lens.lens (\VirtualNodeRef' {virtualNodeName} -> virtualNodeName) (\s@VirtualNodeRef' {} a -> s {virtualNodeName = a} :: VirtualNodeRef)

instance Data.FromJSON VirtualNodeRef where
  parseJSON =
    Data.withObject
      "VirtualNodeRef"
      ( \x ->
          VirtualNodeRef'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "meshName")
            Prelude.<*> (x Data..: "meshOwner")
            Prelude.<*> (x Data..: "resourceOwner")
            Prelude.<*> (x Data..: "version")
            Prelude.<*> (x Data..: "virtualNodeName")
      )

instance Prelude.Hashable VirtualNodeRef where
  hashWithSalt _salt VirtualNodeRef' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualNodeName

instance Prelude.NFData VirtualNodeRef where
  rnf VirtualNodeRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf virtualNodeName

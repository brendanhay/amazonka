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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceRef
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual service returned by a list
-- operation.
--
-- /See:/ 'newVirtualServiceRef' smart constructor.
data VirtualServiceRef = VirtualServiceRef'
  { -- | The full Amazon Resource Name (ARN) for the virtual service.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Data.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Data.POSIX,
    -- | The name of the service mesh that the virtual service resides in.
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
    -- | The name of the virtual service.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'virtualServiceRef_arn' - The full Amazon Resource Name (ARN) for the virtual service.
--
-- 'createdAt', 'virtualServiceRef_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'virtualServiceRef_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshName', 'virtualServiceRef_meshName' - The name of the service mesh that the virtual service resides in.
--
-- 'meshOwner', 'virtualServiceRef_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'virtualServiceRef_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'version', 'virtualServiceRef_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
--
-- 'virtualServiceName', 'virtualServiceRef_virtualServiceName' - The name of the virtual service.
newVirtualServiceRef ::
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
  -- | 'virtualServiceName'
  Prelude.Text ->
  VirtualServiceRef
newVirtualServiceRef
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshName_
  pMeshOwner_
  pResourceOwner_
  pVersion_
  pVirtualServiceName_ =
    VirtualServiceRef'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        meshName = pMeshName_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        version = pVersion_,
        virtualServiceName = pVirtualServiceName_
      }

-- | The full Amazon Resource Name (ARN) for the virtual service.
virtualServiceRef_arn :: Lens.Lens' VirtualServiceRef Prelude.Text
virtualServiceRef_arn = Lens.lens (\VirtualServiceRef' {arn} -> arn) (\s@VirtualServiceRef' {} a -> s {arn = a} :: VirtualServiceRef)

-- | The Unix epoch timestamp in seconds for when the resource was created.
virtualServiceRef_createdAt :: Lens.Lens' VirtualServiceRef Prelude.UTCTime
virtualServiceRef_createdAt = Lens.lens (\VirtualServiceRef' {createdAt} -> createdAt) (\s@VirtualServiceRef' {} a -> s {createdAt = a} :: VirtualServiceRef) Prelude.. Data._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
virtualServiceRef_lastUpdatedAt :: Lens.Lens' VirtualServiceRef Prelude.UTCTime
virtualServiceRef_lastUpdatedAt = Lens.lens (\VirtualServiceRef' {lastUpdatedAt} -> lastUpdatedAt) (\s@VirtualServiceRef' {} a -> s {lastUpdatedAt = a} :: VirtualServiceRef) Prelude.. Data._Time

-- | The name of the service mesh that the virtual service resides in.
virtualServiceRef_meshName :: Lens.Lens' VirtualServiceRef Prelude.Text
virtualServiceRef_meshName = Lens.lens (\VirtualServiceRef' {meshName} -> meshName) (\s@VirtualServiceRef' {} a -> s {meshName = a} :: VirtualServiceRef)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualServiceRef_meshOwner :: Lens.Lens' VirtualServiceRef Prelude.Text
virtualServiceRef_meshOwner = Lens.lens (\VirtualServiceRef' {meshOwner} -> meshOwner) (\s@VirtualServiceRef' {} a -> s {meshOwner = a} :: VirtualServiceRef)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
virtualServiceRef_resourceOwner :: Lens.Lens' VirtualServiceRef Prelude.Text
virtualServiceRef_resourceOwner = Lens.lens (\VirtualServiceRef' {resourceOwner} -> resourceOwner) (\s@VirtualServiceRef' {} a -> s {resourceOwner = a} :: VirtualServiceRef)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
virtualServiceRef_version :: Lens.Lens' VirtualServiceRef Prelude.Integer
virtualServiceRef_version = Lens.lens (\VirtualServiceRef' {version} -> version) (\s@VirtualServiceRef' {} a -> s {version = a} :: VirtualServiceRef)

-- | The name of the virtual service.
virtualServiceRef_virtualServiceName :: Lens.Lens' VirtualServiceRef Prelude.Text
virtualServiceRef_virtualServiceName = Lens.lens (\VirtualServiceRef' {virtualServiceName} -> virtualServiceName) (\s@VirtualServiceRef' {} a -> s {virtualServiceName = a} :: VirtualServiceRef)

instance Data.FromJSON VirtualServiceRef where
  parseJSON =
    Data.withObject
      "VirtualServiceRef"
      ( \x ->
          VirtualServiceRef'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "meshName")
            Prelude.<*> (x Data..: "meshOwner")
            Prelude.<*> (x Data..: "resourceOwner")
            Prelude.<*> (x Data..: "version")
            Prelude.<*> (x Data..: "virtualServiceName")
      )

instance Prelude.Hashable VirtualServiceRef where
  hashWithSalt _salt VirtualServiceRef' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData VirtualServiceRef where
  rnf VirtualServiceRef' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf virtualServiceName

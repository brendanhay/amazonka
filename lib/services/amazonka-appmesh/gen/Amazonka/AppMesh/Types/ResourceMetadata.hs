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
-- Module      : Amazonka.AppMesh.Types.ResourceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ResourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents metadata for a resource.
--
-- /See:/ 'newResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { -- | The full Amazon Resource Name (ARN) for the resource.
    arn :: Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Data.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was last
    -- updated.
    lastUpdatedAt :: Data.POSIX,
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
    -- | The unique identifier for the resource.
    uid :: Prelude.Text,
    -- | The version of the resource. Resources are created at version 1, and
    -- this version is incremented each time that they\'re updated.
    version :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceMetadata_arn' - The full Amazon Resource Name (ARN) for the resource.
--
-- 'createdAt', 'resourceMetadata_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'lastUpdatedAt', 'resourceMetadata_lastUpdatedAt' - The Unix epoch timestamp in seconds for when the resource was last
-- updated.
--
-- 'meshOwner', 'resourceMetadata_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'resourceOwner', 'resourceMetadata_resourceOwner' - The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'uid', 'resourceMetadata_uid' - The unique identifier for the resource.
--
-- 'version', 'resourceMetadata_version' - The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
newResourceMetadata ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'meshOwner'
  Prelude.Text ->
  -- | 'resourceOwner'
  Prelude.Text ->
  -- | 'uid'
  Prelude.Text ->
  -- | 'version'
  Prelude.Integer ->
  ResourceMetadata
newResourceMetadata
  pArn_
  pCreatedAt_
  pLastUpdatedAt_
  pMeshOwner_
  pResourceOwner_
  pUid_
  pVersion_ =
    ResourceMetadata'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        meshOwner = pMeshOwner_,
        resourceOwner = pResourceOwner_,
        uid = pUid_,
        version = pVersion_
      }

-- | The full Amazon Resource Name (ARN) for the resource.
resourceMetadata_arn :: Lens.Lens' ResourceMetadata Prelude.Text
resourceMetadata_arn = Lens.lens (\ResourceMetadata' {arn} -> arn) (\s@ResourceMetadata' {} a -> s {arn = a} :: ResourceMetadata)

-- | The Unix epoch timestamp in seconds for when the resource was created.
resourceMetadata_createdAt :: Lens.Lens' ResourceMetadata Prelude.UTCTime
resourceMetadata_createdAt = Lens.lens (\ResourceMetadata' {createdAt} -> createdAt) (\s@ResourceMetadata' {} a -> s {createdAt = a} :: ResourceMetadata) Prelude.. Data._Time

-- | The Unix epoch timestamp in seconds for when the resource was last
-- updated.
resourceMetadata_lastUpdatedAt :: Lens.Lens' ResourceMetadata Prelude.UTCTime
resourceMetadata_lastUpdatedAt = Lens.lens (\ResourceMetadata' {lastUpdatedAt} -> lastUpdatedAt) (\s@ResourceMetadata' {} a -> s {lastUpdatedAt = a} :: ResourceMetadata) Prelude.. Data._Time

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
resourceMetadata_meshOwner :: Lens.Lens' ResourceMetadata Prelude.Text
resourceMetadata_meshOwner = Lens.lens (\ResourceMetadata' {meshOwner} -> meshOwner) (\s@ResourceMetadata' {} a -> s {meshOwner = a} :: ResourceMetadata)

-- | The Amazon Web Services IAM account ID of the resource owner. If the
-- account ID is not your own, then it\'s the ID of the mesh owner or of
-- another account that the mesh is shared with. For more information about
-- mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
resourceMetadata_resourceOwner :: Lens.Lens' ResourceMetadata Prelude.Text
resourceMetadata_resourceOwner = Lens.lens (\ResourceMetadata' {resourceOwner} -> resourceOwner) (\s@ResourceMetadata' {} a -> s {resourceOwner = a} :: ResourceMetadata)

-- | The unique identifier for the resource.
resourceMetadata_uid :: Lens.Lens' ResourceMetadata Prelude.Text
resourceMetadata_uid = Lens.lens (\ResourceMetadata' {uid} -> uid) (\s@ResourceMetadata' {} a -> s {uid = a} :: ResourceMetadata)

-- | The version of the resource. Resources are created at version 1, and
-- this version is incremented each time that they\'re updated.
resourceMetadata_version :: Lens.Lens' ResourceMetadata Prelude.Integer
resourceMetadata_version = Lens.lens (\ResourceMetadata' {version} -> version) (\s@ResourceMetadata' {} a -> s {version = a} :: ResourceMetadata)

instance Data.FromJSON ResourceMetadata where
  parseJSON =
    Data.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "meshOwner")
            Prelude.<*> (x Data..: "resourceOwner")
            Prelude.<*> (x Data..: "uid")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable ResourceMetadata where
  hashWithSalt _salt ResourceMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` version

instance Prelude.NFData ResourceMetadata where
  rnf ResourceMetadata' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf uid
      `Prelude.seq` Prelude.rnf version

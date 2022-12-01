{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageVOD.CreateAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MediaPackage VOD Asset resource.
module Amazonka.MediaPackageVOD.CreateAsset
  ( -- * Creating a Request
    CreateAsset (..),
    newCreateAsset,

    -- * Request Lenses
    createAsset_resourceId,
    createAsset_tags,
    createAsset_sourceArn,
    createAsset_id,
    createAsset_packagingGroupId,
    createAsset_sourceRoleArn,

    -- * Destructuring the Response
    CreateAssetResponse (..),
    newCreateAssetResponse,

    -- * Response Lenses
    createAssetResponse_resourceId,
    createAssetResponse_tags,
    createAssetResponse_sourceRoleArn,
    createAssetResponse_sourceArn,
    createAssetResponse_packagingGroupId,
    createAssetResponse_arn,
    createAssetResponse_id,
    createAssetResponse_createdAt,
    createAssetResponse_egressEndpoints,
    createAssetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A new MediaPackage VOD Asset configuration.
--
-- /See:/ 'newCreateAsset' smart constructor.
data CreateAsset = CreateAsset'
  { -- | The resource ID to include in SPEKE key requests.
    resourceId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ARN of the source object in S3.
    sourceArn :: Prelude.Text,
    -- | The unique identifier for the Asset.
    id :: Prelude.Text,
    -- | The ID of the PackagingGroup for the Asset.
    packagingGroupId :: Prelude.Text,
    -- | The IAM role ARN used to access the source S3 bucket.
    sourceRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'createAsset_resourceId' - The resource ID to include in SPEKE key requests.
--
-- 'tags', 'createAsset_tags' - Undocumented member.
--
-- 'sourceArn', 'createAsset_sourceArn' - ARN of the source object in S3.
--
-- 'id', 'createAsset_id' - The unique identifier for the Asset.
--
-- 'packagingGroupId', 'createAsset_packagingGroupId' - The ID of the PackagingGroup for the Asset.
--
-- 'sourceRoleArn', 'createAsset_sourceRoleArn' - The IAM role ARN used to access the source S3 bucket.
newCreateAsset ::
  -- | 'sourceArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'packagingGroupId'
  Prelude.Text ->
  -- | 'sourceRoleArn'
  Prelude.Text ->
  CreateAsset
newCreateAsset
  pSourceArn_
  pId_
  pPackagingGroupId_
  pSourceRoleArn_ =
    CreateAsset'
      { resourceId = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceArn = pSourceArn_,
        id = pId_,
        packagingGroupId = pPackagingGroupId_,
        sourceRoleArn = pSourceRoleArn_
      }

-- | The resource ID to include in SPEKE key requests.
createAsset_resourceId :: Lens.Lens' CreateAsset (Prelude.Maybe Prelude.Text)
createAsset_resourceId = Lens.lens (\CreateAsset' {resourceId} -> resourceId) (\s@CreateAsset' {} a -> s {resourceId = a} :: CreateAsset)

-- | Undocumented member.
createAsset_tags :: Lens.Lens' CreateAsset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAsset_tags = Lens.lens (\CreateAsset' {tags} -> tags) (\s@CreateAsset' {} a -> s {tags = a} :: CreateAsset) Prelude.. Lens.mapping Lens.coerced

-- | ARN of the source object in S3.
createAsset_sourceArn :: Lens.Lens' CreateAsset Prelude.Text
createAsset_sourceArn = Lens.lens (\CreateAsset' {sourceArn} -> sourceArn) (\s@CreateAsset' {} a -> s {sourceArn = a} :: CreateAsset)

-- | The unique identifier for the Asset.
createAsset_id :: Lens.Lens' CreateAsset Prelude.Text
createAsset_id = Lens.lens (\CreateAsset' {id} -> id) (\s@CreateAsset' {} a -> s {id = a} :: CreateAsset)

-- | The ID of the PackagingGroup for the Asset.
createAsset_packagingGroupId :: Lens.Lens' CreateAsset Prelude.Text
createAsset_packagingGroupId = Lens.lens (\CreateAsset' {packagingGroupId} -> packagingGroupId) (\s@CreateAsset' {} a -> s {packagingGroupId = a} :: CreateAsset)

-- | The IAM role ARN used to access the source S3 bucket.
createAsset_sourceRoleArn :: Lens.Lens' CreateAsset Prelude.Text
createAsset_sourceRoleArn = Lens.lens (\CreateAsset' {sourceRoleArn} -> sourceRoleArn) (\s@CreateAsset' {} a -> s {sourceRoleArn = a} :: CreateAsset)

instance Core.AWSRequest CreateAsset where
  type AWSResponse CreateAsset = CreateAssetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssetResponse'
            Prelude.<$> (x Core..?> "resourceId")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "sourceRoleArn")
            Prelude.<*> (x Core..?> "sourceArn")
            Prelude.<*> (x Core..?> "packagingGroupId")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> ( x Core..?> "egressEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAsset where
  hashWithSalt _salt CreateAsset' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` packagingGroupId
      `Prelude.hashWithSalt` sourceRoleArn

instance Prelude.NFData CreateAsset where
  rnf CreateAsset' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf packagingGroupId
      `Prelude.seq` Prelude.rnf sourceRoleArn

instance Core.ToHeaders CreateAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAsset where
  toJSON CreateAsset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceId" Core..=) Prelude.<$> resourceId,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("sourceArn" Core..= sourceArn),
            Prelude.Just ("id" Core..= id),
            Prelude.Just
              ("packagingGroupId" Core..= packagingGroupId),
            Prelude.Just
              ("sourceRoleArn" Core..= sourceRoleArn)
          ]
      )

instance Core.ToPath CreateAsset where
  toPath = Prelude.const "/assets"

instance Core.ToQuery CreateAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssetResponse' smart constructor.
data CreateAssetResponse = CreateAssetResponse'
  { -- | The resource ID to include in SPEKE key requests.
    resourceId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The IAM role_arn used to access the source S3 bucket.
    sourceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | ARN of the source object in S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingGroup for the Asset.
    packagingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Asset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Asset.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time the Asset was initially submitted for Ingest.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The list of egress endpoints available for the Asset.
    egressEndpoints :: Prelude.Maybe [EgressEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'createAssetResponse_resourceId' - The resource ID to include in SPEKE key requests.
--
-- 'tags', 'createAssetResponse_tags' - Undocumented member.
--
-- 'sourceRoleArn', 'createAssetResponse_sourceRoleArn' - The IAM role_arn used to access the source S3 bucket.
--
-- 'sourceArn', 'createAssetResponse_sourceArn' - ARN of the source object in S3.
--
-- 'packagingGroupId', 'createAssetResponse_packagingGroupId' - The ID of the PackagingGroup for the Asset.
--
-- 'arn', 'createAssetResponse_arn' - The ARN of the Asset.
--
-- 'id', 'createAssetResponse_id' - The unique identifier for the Asset.
--
-- 'createdAt', 'createAssetResponse_createdAt' - The time the Asset was initially submitted for Ingest.
--
-- 'egressEndpoints', 'createAssetResponse_egressEndpoints' - The list of egress endpoints available for the Asset.
--
-- 'httpStatus', 'createAssetResponse_httpStatus' - The response's http status code.
newCreateAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssetResponse
newCreateAssetResponse pHttpStatus_ =
  CreateAssetResponse'
    { resourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceRoleArn = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      packagingGroupId = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource ID to include in SPEKE key requests.
createAssetResponse_resourceId :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_resourceId = Lens.lens (\CreateAssetResponse' {resourceId} -> resourceId) (\s@CreateAssetResponse' {} a -> s {resourceId = a} :: CreateAssetResponse)

-- | Undocumented member.
createAssetResponse_tags :: Lens.Lens' CreateAssetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssetResponse_tags = Lens.lens (\CreateAssetResponse' {tags} -> tags) (\s@CreateAssetResponse' {} a -> s {tags = a} :: CreateAssetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role_arn used to access the source S3 bucket.
createAssetResponse_sourceRoleArn :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_sourceRoleArn = Lens.lens (\CreateAssetResponse' {sourceRoleArn} -> sourceRoleArn) (\s@CreateAssetResponse' {} a -> s {sourceRoleArn = a} :: CreateAssetResponse)

-- | ARN of the source object in S3.
createAssetResponse_sourceArn :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_sourceArn = Lens.lens (\CreateAssetResponse' {sourceArn} -> sourceArn) (\s@CreateAssetResponse' {} a -> s {sourceArn = a} :: CreateAssetResponse)

-- | The ID of the PackagingGroup for the Asset.
createAssetResponse_packagingGroupId :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_packagingGroupId = Lens.lens (\CreateAssetResponse' {packagingGroupId} -> packagingGroupId) (\s@CreateAssetResponse' {} a -> s {packagingGroupId = a} :: CreateAssetResponse)

-- | The ARN of the Asset.
createAssetResponse_arn :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_arn = Lens.lens (\CreateAssetResponse' {arn} -> arn) (\s@CreateAssetResponse' {} a -> s {arn = a} :: CreateAssetResponse)

-- | The unique identifier for the Asset.
createAssetResponse_id :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_id = Lens.lens (\CreateAssetResponse' {id} -> id) (\s@CreateAssetResponse' {} a -> s {id = a} :: CreateAssetResponse)

-- | The time the Asset was initially submitted for Ingest.
createAssetResponse_createdAt :: Lens.Lens' CreateAssetResponse (Prelude.Maybe Prelude.Text)
createAssetResponse_createdAt = Lens.lens (\CreateAssetResponse' {createdAt} -> createdAt) (\s@CreateAssetResponse' {} a -> s {createdAt = a} :: CreateAssetResponse)

-- | The list of egress endpoints available for the Asset.
createAssetResponse_egressEndpoints :: Lens.Lens' CreateAssetResponse (Prelude.Maybe [EgressEndpoint])
createAssetResponse_egressEndpoints = Lens.lens (\CreateAssetResponse' {egressEndpoints} -> egressEndpoints) (\s@CreateAssetResponse' {} a -> s {egressEndpoints = a} :: CreateAssetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createAssetResponse_httpStatus :: Lens.Lens' CreateAssetResponse Prelude.Int
createAssetResponse_httpStatus = Lens.lens (\CreateAssetResponse' {httpStatus} -> httpStatus) (\s@CreateAssetResponse' {} a -> s {httpStatus = a} :: CreateAssetResponse)

instance Prelude.NFData CreateAssetResponse where
  rnf CreateAssetResponse' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceRoleArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf packagingGroupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf egressEndpoints
      `Prelude.seq` Prelude.rnf httpStatus

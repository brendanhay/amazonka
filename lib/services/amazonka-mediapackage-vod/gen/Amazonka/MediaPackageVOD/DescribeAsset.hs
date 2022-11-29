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
-- Module      : Amazonka.MediaPackageVOD.DescribeAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a MediaPackage VOD Asset resource.
module Amazonka.MediaPackageVOD.DescribeAsset
  ( -- * Creating a Request
    DescribeAsset (..),
    newDescribeAsset,

    -- * Request Lenses
    describeAsset_id,

    -- * Destructuring the Response
    DescribeAssetResponse (..),
    newDescribeAssetResponse,

    -- * Response Lenses
    describeAssetResponse_resourceId,
    describeAssetResponse_tags,
    describeAssetResponse_sourceRoleArn,
    describeAssetResponse_sourceArn,
    describeAssetResponse_packagingGroupId,
    describeAssetResponse_arn,
    describeAssetResponse_id,
    describeAssetResponse_createdAt,
    describeAssetResponse_egressEndpoints,
    describeAssetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAsset' smart constructor.
data DescribeAsset = DescribeAsset'
  { -- | The ID of an MediaPackage VOD Asset resource.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeAsset_id' - The ID of an MediaPackage VOD Asset resource.
newDescribeAsset ::
  -- | 'id'
  Prelude.Text ->
  DescribeAsset
newDescribeAsset pId_ = DescribeAsset' {id = pId_}

-- | The ID of an MediaPackage VOD Asset resource.
describeAsset_id :: Lens.Lens' DescribeAsset Prelude.Text
describeAsset_id = Lens.lens (\DescribeAsset' {id} -> id) (\s@DescribeAsset' {} a -> s {id = a} :: DescribeAsset)

instance Core.AWSRequest DescribeAsset where
  type
    AWSResponse DescribeAsset =
      DescribeAssetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetResponse'
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

instance Prelude.Hashable DescribeAsset where
  hashWithSalt _salt DescribeAsset' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeAsset where
  rnf DescribeAsset' {..} = Prelude.rnf id

instance Core.ToHeaders DescribeAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAsset where
  toPath DescribeAsset' {..} =
    Prelude.mconcat ["/assets/", Core.toBS id]

instance Core.ToQuery DescribeAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssetResponse' smart constructor.
data DescribeAssetResponse = DescribeAssetResponse'
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
-- Create a value of 'DescribeAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeAssetResponse_resourceId' - The resource ID to include in SPEKE key requests.
--
-- 'tags', 'describeAssetResponse_tags' - Undocumented member.
--
-- 'sourceRoleArn', 'describeAssetResponse_sourceRoleArn' - The IAM role_arn used to access the source S3 bucket.
--
-- 'sourceArn', 'describeAssetResponse_sourceArn' - ARN of the source object in S3.
--
-- 'packagingGroupId', 'describeAssetResponse_packagingGroupId' - The ID of the PackagingGroup for the Asset.
--
-- 'arn', 'describeAssetResponse_arn' - The ARN of the Asset.
--
-- 'id', 'describeAssetResponse_id' - The unique identifier for the Asset.
--
-- 'createdAt', 'describeAssetResponse_createdAt' - The time the Asset was initially submitted for Ingest.
--
-- 'egressEndpoints', 'describeAssetResponse_egressEndpoints' - The list of egress endpoints available for the Asset.
--
-- 'httpStatus', 'describeAssetResponse_httpStatus' - The response's http status code.
newDescribeAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAssetResponse
newDescribeAssetResponse pHttpStatus_ =
  DescribeAssetResponse'
    { resourceId =
        Prelude.Nothing,
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
describeAssetResponse_resourceId :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_resourceId = Lens.lens (\DescribeAssetResponse' {resourceId} -> resourceId) (\s@DescribeAssetResponse' {} a -> s {resourceId = a} :: DescribeAssetResponse)

-- | Undocumented member.
describeAssetResponse_tags :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeAssetResponse_tags = Lens.lens (\DescribeAssetResponse' {tags} -> tags) (\s@DescribeAssetResponse' {} a -> s {tags = a} :: DescribeAssetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role_arn used to access the source S3 bucket.
describeAssetResponse_sourceRoleArn :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_sourceRoleArn = Lens.lens (\DescribeAssetResponse' {sourceRoleArn} -> sourceRoleArn) (\s@DescribeAssetResponse' {} a -> s {sourceRoleArn = a} :: DescribeAssetResponse)

-- | ARN of the source object in S3.
describeAssetResponse_sourceArn :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_sourceArn = Lens.lens (\DescribeAssetResponse' {sourceArn} -> sourceArn) (\s@DescribeAssetResponse' {} a -> s {sourceArn = a} :: DescribeAssetResponse)

-- | The ID of the PackagingGroup for the Asset.
describeAssetResponse_packagingGroupId :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_packagingGroupId = Lens.lens (\DescribeAssetResponse' {packagingGroupId} -> packagingGroupId) (\s@DescribeAssetResponse' {} a -> s {packagingGroupId = a} :: DescribeAssetResponse)

-- | The ARN of the Asset.
describeAssetResponse_arn :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_arn = Lens.lens (\DescribeAssetResponse' {arn} -> arn) (\s@DescribeAssetResponse' {} a -> s {arn = a} :: DescribeAssetResponse)

-- | The unique identifier for the Asset.
describeAssetResponse_id :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_id = Lens.lens (\DescribeAssetResponse' {id} -> id) (\s@DescribeAssetResponse' {} a -> s {id = a} :: DescribeAssetResponse)

-- | The time the Asset was initially submitted for Ingest.
describeAssetResponse_createdAt :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_createdAt = Lens.lens (\DescribeAssetResponse' {createdAt} -> createdAt) (\s@DescribeAssetResponse' {} a -> s {createdAt = a} :: DescribeAssetResponse)

-- | The list of egress endpoints available for the Asset.
describeAssetResponse_egressEndpoints :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe [EgressEndpoint])
describeAssetResponse_egressEndpoints = Lens.lens (\DescribeAssetResponse' {egressEndpoints} -> egressEndpoints) (\s@DescribeAssetResponse' {} a -> s {egressEndpoints = a} :: DescribeAssetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAssetResponse_httpStatus :: Lens.Lens' DescribeAssetResponse Prelude.Int
describeAssetResponse_httpStatus = Lens.lens (\DescribeAssetResponse' {httpStatus} -> httpStatus) (\s@DescribeAssetResponse' {} a -> s {httpStatus = a} :: DescribeAssetResponse)

instance Prelude.NFData DescribeAssetResponse where
  rnf DescribeAssetResponse' {..} =
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

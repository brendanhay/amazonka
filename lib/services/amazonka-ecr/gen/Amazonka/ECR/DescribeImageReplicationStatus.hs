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
-- Module      : Amazonka.ECR.DescribeImageReplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the replication status for a specified image.
module Amazonka.ECR.DescribeImageReplicationStatus
  ( -- * Creating a Request
    DescribeImageReplicationStatus (..),
    newDescribeImageReplicationStatus,

    -- * Request Lenses
    describeImageReplicationStatus_registryId,
    describeImageReplicationStatus_repositoryName,
    describeImageReplicationStatus_imageId,

    -- * Destructuring the Response
    DescribeImageReplicationStatusResponse (..),
    newDescribeImageReplicationStatusResponse,

    -- * Response Lenses
    describeImageReplicationStatusResponse_imageId,
    describeImageReplicationStatusResponse_repositoryName,
    describeImageReplicationStatusResponse_replicationStatuses,
    describeImageReplicationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImageReplicationStatus' smart constructor.
data DescribeImageReplicationStatus = DescribeImageReplicationStatus'
  { -- | The Amazon Web Services account ID associated with the registry. If you
    -- do not specify a registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that the image is in.
    repositoryName :: Prelude.Text,
    imageId :: ImageIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageReplicationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'describeImageReplicationStatus_registryId' - The Amazon Web Services account ID associated with the registry. If you
-- do not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'describeImageReplicationStatus_repositoryName' - The name of the repository that the image is in.
--
-- 'imageId', 'describeImageReplicationStatus_imageId' - Undocumented member.
newDescribeImageReplicationStatus ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'imageId'
  ImageIdentifier ->
  DescribeImageReplicationStatus
newDescribeImageReplicationStatus
  pRepositoryName_
  pImageId_ =
    DescribeImageReplicationStatus'
      { registryId =
          Prelude.Nothing,
        repositoryName = pRepositoryName_,
        imageId = pImageId_
      }

-- | The Amazon Web Services account ID associated with the registry. If you
-- do not specify a registry, the default registry is assumed.
describeImageReplicationStatus_registryId :: Lens.Lens' DescribeImageReplicationStatus (Prelude.Maybe Prelude.Text)
describeImageReplicationStatus_registryId = Lens.lens (\DescribeImageReplicationStatus' {registryId} -> registryId) (\s@DescribeImageReplicationStatus' {} a -> s {registryId = a} :: DescribeImageReplicationStatus)

-- | The name of the repository that the image is in.
describeImageReplicationStatus_repositoryName :: Lens.Lens' DescribeImageReplicationStatus Prelude.Text
describeImageReplicationStatus_repositoryName = Lens.lens (\DescribeImageReplicationStatus' {repositoryName} -> repositoryName) (\s@DescribeImageReplicationStatus' {} a -> s {repositoryName = a} :: DescribeImageReplicationStatus)

-- | Undocumented member.
describeImageReplicationStatus_imageId :: Lens.Lens' DescribeImageReplicationStatus ImageIdentifier
describeImageReplicationStatus_imageId = Lens.lens (\DescribeImageReplicationStatus' {imageId} -> imageId) (\s@DescribeImageReplicationStatus' {} a -> s {imageId = a} :: DescribeImageReplicationStatus)

instance
  Core.AWSRequest
    DescribeImageReplicationStatus
  where
  type
    AWSResponse DescribeImageReplicationStatus =
      DescribeImageReplicationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageReplicationStatusResponse'
            Prelude.<$> (x Core..?> "imageId")
            Prelude.<*> (x Core..?> "repositoryName")
            Prelude.<*> ( x Core..?> "replicationStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeImageReplicationStatus
  where
  hashWithSalt
    salt'
    DescribeImageReplicationStatus' {..} =
      salt' `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` repositoryName
        `Prelude.hashWithSalt` registryId

instance
  Prelude.NFData
    DescribeImageReplicationStatus
  where
  rnf DescribeImageReplicationStatus' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf repositoryName

instance
  Core.ToHeaders
    DescribeImageReplicationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageReplicationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImageReplicationStatus where
  toJSON DescribeImageReplicationStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("imageId" Core..= imageId)
          ]
      )

instance Core.ToPath DescribeImageReplicationStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImageReplicationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageReplicationStatusResponse' smart constructor.
data DescribeImageReplicationStatusResponse = DescribeImageReplicationStatusResponse'
  { imageId :: Prelude.Maybe ImageIdentifier,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The replication status details for the images in the specified
    -- repository.
    replicationStatuses :: Prelude.Maybe [ImageReplicationStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageReplicationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'describeImageReplicationStatusResponse_imageId' - Undocumented member.
--
-- 'repositoryName', 'describeImageReplicationStatusResponse_repositoryName' - The repository name associated with the request.
--
-- 'replicationStatuses', 'describeImageReplicationStatusResponse_replicationStatuses' - The replication status details for the images in the specified
-- repository.
--
-- 'httpStatus', 'describeImageReplicationStatusResponse_httpStatus' - The response's http status code.
newDescribeImageReplicationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageReplicationStatusResponse
newDescribeImageReplicationStatusResponse
  pHttpStatus_ =
    DescribeImageReplicationStatusResponse'
      { imageId =
          Prelude.Nothing,
        repositoryName = Prelude.Nothing,
        replicationStatuses =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeImageReplicationStatusResponse_imageId :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe ImageIdentifier)
describeImageReplicationStatusResponse_imageId = Lens.lens (\DescribeImageReplicationStatusResponse' {imageId} -> imageId) (\s@DescribeImageReplicationStatusResponse' {} a -> s {imageId = a} :: DescribeImageReplicationStatusResponse)

-- | The repository name associated with the request.
describeImageReplicationStatusResponse_repositoryName :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe Prelude.Text)
describeImageReplicationStatusResponse_repositoryName = Lens.lens (\DescribeImageReplicationStatusResponse' {repositoryName} -> repositoryName) (\s@DescribeImageReplicationStatusResponse' {} a -> s {repositoryName = a} :: DescribeImageReplicationStatusResponse)

-- | The replication status details for the images in the specified
-- repository.
describeImageReplicationStatusResponse_replicationStatuses :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe [ImageReplicationStatus])
describeImageReplicationStatusResponse_replicationStatuses = Lens.lens (\DescribeImageReplicationStatusResponse' {replicationStatuses} -> replicationStatuses) (\s@DescribeImageReplicationStatusResponse' {} a -> s {replicationStatuses = a} :: DescribeImageReplicationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImageReplicationStatusResponse_httpStatus :: Lens.Lens' DescribeImageReplicationStatusResponse Prelude.Int
describeImageReplicationStatusResponse_httpStatus = Lens.lens (\DescribeImageReplicationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeImageReplicationStatusResponse' {} a -> s {httpStatus = a} :: DescribeImageReplicationStatusResponse)

instance
  Prelude.NFData
    DescribeImageReplicationStatusResponse
  where
  rnf DescribeImageReplicationStatusResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf replicationStatuses
      `Prelude.seq` Prelude.rnf repositoryName

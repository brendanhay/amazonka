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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeImageReplicationStatusResponse_replicationStatuses,
    describeImageReplicationStatusResponse_repositoryName,
    describeImageReplicationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageReplicationStatusResponse'
            Prelude.<$> (x Data..?> "imageId")
            Prelude.<*> ( x
                            Data..?> "replicationStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeImageReplicationStatus
  where
  hashWithSalt
    _salt
    DescribeImageReplicationStatus' {..} =
      _salt
        `Prelude.hashWithSalt` registryId
        `Prelude.hashWithSalt` repositoryName
        `Prelude.hashWithSalt` imageId

instance
  Prelude.NFData
    DescribeImageReplicationStatus
  where
  rnf DescribeImageReplicationStatus' {..} =
    Prelude.rnf registryId `Prelude.seq`
      Prelude.rnf repositoryName `Prelude.seq`
        Prelude.rnf imageId

instance
  Data.ToHeaders
    DescribeImageReplicationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImageReplicationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImageReplicationStatus where
  toJSON DescribeImageReplicationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("imageId" Data..= imageId)
          ]
      )

instance Data.ToPath DescribeImageReplicationStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImageReplicationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageReplicationStatusResponse' smart constructor.
data DescribeImageReplicationStatusResponse = DescribeImageReplicationStatusResponse'
  { imageId :: Prelude.Maybe ImageIdentifier,
    -- | The replication status details for the images in the specified
    -- repository.
    replicationStatuses :: Prelude.Maybe [ImageReplicationStatus],
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
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
-- 'replicationStatuses', 'describeImageReplicationStatusResponse_replicationStatuses' - The replication status details for the images in the specified
-- repository.
--
-- 'repositoryName', 'describeImageReplicationStatusResponse_repositoryName' - The repository name associated with the request.
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
        replicationStatuses =
          Prelude.Nothing,
        repositoryName = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeImageReplicationStatusResponse_imageId :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe ImageIdentifier)
describeImageReplicationStatusResponse_imageId = Lens.lens (\DescribeImageReplicationStatusResponse' {imageId} -> imageId) (\s@DescribeImageReplicationStatusResponse' {} a -> s {imageId = a} :: DescribeImageReplicationStatusResponse)

-- | The replication status details for the images in the specified
-- repository.
describeImageReplicationStatusResponse_replicationStatuses :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe [ImageReplicationStatus])
describeImageReplicationStatusResponse_replicationStatuses = Lens.lens (\DescribeImageReplicationStatusResponse' {replicationStatuses} -> replicationStatuses) (\s@DescribeImageReplicationStatusResponse' {} a -> s {replicationStatuses = a} :: DescribeImageReplicationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The repository name associated with the request.
describeImageReplicationStatusResponse_repositoryName :: Lens.Lens' DescribeImageReplicationStatusResponse (Prelude.Maybe Prelude.Text)
describeImageReplicationStatusResponse_repositoryName = Lens.lens (\DescribeImageReplicationStatusResponse' {repositoryName} -> repositoryName) (\s@DescribeImageReplicationStatusResponse' {} a -> s {repositoryName = a} :: DescribeImageReplicationStatusResponse)

-- | The response's http status code.
describeImageReplicationStatusResponse_httpStatus :: Lens.Lens' DescribeImageReplicationStatusResponse Prelude.Int
describeImageReplicationStatusResponse_httpStatus = Lens.lens (\DescribeImageReplicationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeImageReplicationStatusResponse' {} a -> s {httpStatus = a} :: DescribeImageReplicationStatusResponse)

instance
  Prelude.NFData
    DescribeImageReplicationStatusResponse
  where
  rnf DescribeImageReplicationStatusResponse' {..} =
    Prelude.rnf imageId `Prelude.seq`
      Prelude.rnf replicationStatuses `Prelude.seq`
        Prelude.rnf repositoryName `Prelude.seq`
          Prelude.rnf httpStatus

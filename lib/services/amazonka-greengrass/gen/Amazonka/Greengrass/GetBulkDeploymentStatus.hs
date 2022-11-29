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
-- Module      : Amazonka.Greengrass.GetBulkDeploymentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a bulk deployment.
module Amazonka.Greengrass.GetBulkDeploymentStatus
  ( -- * Creating a Request
    GetBulkDeploymentStatus (..),
    newGetBulkDeploymentStatus,

    -- * Request Lenses
    getBulkDeploymentStatus_bulkDeploymentId,

    -- * Destructuring the Response
    GetBulkDeploymentStatusResponse (..),
    newGetBulkDeploymentStatusResponse,

    -- * Response Lenses
    getBulkDeploymentStatusResponse_tags,
    getBulkDeploymentStatusResponse_errorMessage,
    getBulkDeploymentStatusResponse_errorDetails,
    getBulkDeploymentStatusResponse_bulkDeploymentStatus,
    getBulkDeploymentStatusResponse_bulkDeploymentMetrics,
    getBulkDeploymentStatusResponse_createdAt,
    getBulkDeploymentStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBulkDeploymentStatus' smart constructor.
data GetBulkDeploymentStatus = GetBulkDeploymentStatus'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBulkDeploymentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeploymentId', 'getBulkDeploymentStatus_bulkDeploymentId' - The ID of the bulk deployment.
newGetBulkDeploymentStatus ::
  -- | 'bulkDeploymentId'
  Prelude.Text ->
  GetBulkDeploymentStatus
newGetBulkDeploymentStatus pBulkDeploymentId_ =
  GetBulkDeploymentStatus'
    { bulkDeploymentId =
        pBulkDeploymentId_
    }

-- | The ID of the bulk deployment.
getBulkDeploymentStatus_bulkDeploymentId :: Lens.Lens' GetBulkDeploymentStatus Prelude.Text
getBulkDeploymentStatus_bulkDeploymentId = Lens.lens (\GetBulkDeploymentStatus' {bulkDeploymentId} -> bulkDeploymentId) (\s@GetBulkDeploymentStatus' {} a -> s {bulkDeploymentId = a} :: GetBulkDeploymentStatus)

instance Core.AWSRequest GetBulkDeploymentStatus where
  type
    AWSResponse GetBulkDeploymentStatus =
      GetBulkDeploymentStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBulkDeploymentStatusResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "ErrorDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "BulkDeploymentStatus")
            Prelude.<*> (x Core..?> "BulkDeploymentMetrics")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBulkDeploymentStatus where
  hashWithSalt _salt GetBulkDeploymentStatus' {..} =
    _salt `Prelude.hashWithSalt` bulkDeploymentId

instance Prelude.NFData GetBulkDeploymentStatus where
  rnf GetBulkDeploymentStatus' {..} =
    Prelude.rnf bulkDeploymentId

instance Core.ToHeaders GetBulkDeploymentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBulkDeploymentStatus where
  toPath GetBulkDeploymentStatus' {..} =
    Prelude.mconcat
      [ "/greengrass/bulk/deployments/",
        Core.toBS bulkDeploymentId,
        "/status"
      ]

instance Core.ToQuery GetBulkDeploymentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBulkDeploymentStatusResponse' smart constructor.
data GetBulkDeploymentStatusResponse = GetBulkDeploymentStatusResponse'
  { -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Error message
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Error details
    errorDetails :: Prelude.Maybe [ErrorDetail],
    -- | The status of the bulk deployment.
    bulkDeploymentStatus :: Prelude.Maybe BulkDeploymentStatus,
    -- | Relevant metrics on input records processed during bulk deployment.
    bulkDeploymentMetrics :: Prelude.Maybe BulkDeploymentMetrics,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBulkDeploymentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getBulkDeploymentStatusResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'errorMessage', 'getBulkDeploymentStatusResponse_errorMessage' - Error message
--
-- 'errorDetails', 'getBulkDeploymentStatusResponse_errorDetails' - Error details
--
-- 'bulkDeploymentStatus', 'getBulkDeploymentStatusResponse_bulkDeploymentStatus' - The status of the bulk deployment.
--
-- 'bulkDeploymentMetrics', 'getBulkDeploymentStatusResponse_bulkDeploymentMetrics' - Relevant metrics on input records processed during bulk deployment.
--
-- 'createdAt', 'getBulkDeploymentStatusResponse_createdAt' - The time, in ISO format, when the deployment was created.
--
-- 'httpStatus', 'getBulkDeploymentStatusResponse_httpStatus' - The response's http status code.
newGetBulkDeploymentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBulkDeploymentStatusResponse
newGetBulkDeploymentStatusResponse pHttpStatus_ =
  GetBulkDeploymentStatusResponse'
    { tags =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      bulkDeploymentStatus = Prelude.Nothing,
      bulkDeploymentMetrics = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tag(s) attached to the resource arn.
getBulkDeploymentStatusResponse_tags :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getBulkDeploymentStatusResponse_tags = Lens.lens (\GetBulkDeploymentStatusResponse' {tags} -> tags) (\s@GetBulkDeploymentStatusResponse' {} a -> s {tags = a} :: GetBulkDeploymentStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | Error message
getBulkDeploymentStatusResponse_errorMessage :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe Prelude.Text)
getBulkDeploymentStatusResponse_errorMessage = Lens.lens (\GetBulkDeploymentStatusResponse' {errorMessage} -> errorMessage) (\s@GetBulkDeploymentStatusResponse' {} a -> s {errorMessage = a} :: GetBulkDeploymentStatusResponse)

-- | Error details
getBulkDeploymentStatusResponse_errorDetails :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe [ErrorDetail])
getBulkDeploymentStatusResponse_errorDetails = Lens.lens (\GetBulkDeploymentStatusResponse' {errorDetails} -> errorDetails) (\s@GetBulkDeploymentStatusResponse' {} a -> s {errorDetails = a} :: GetBulkDeploymentStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the bulk deployment.
getBulkDeploymentStatusResponse_bulkDeploymentStatus :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe BulkDeploymentStatus)
getBulkDeploymentStatusResponse_bulkDeploymentStatus = Lens.lens (\GetBulkDeploymentStatusResponse' {bulkDeploymentStatus} -> bulkDeploymentStatus) (\s@GetBulkDeploymentStatusResponse' {} a -> s {bulkDeploymentStatus = a} :: GetBulkDeploymentStatusResponse)

-- | Relevant metrics on input records processed during bulk deployment.
getBulkDeploymentStatusResponse_bulkDeploymentMetrics :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe BulkDeploymentMetrics)
getBulkDeploymentStatusResponse_bulkDeploymentMetrics = Lens.lens (\GetBulkDeploymentStatusResponse' {bulkDeploymentMetrics} -> bulkDeploymentMetrics) (\s@GetBulkDeploymentStatusResponse' {} a -> s {bulkDeploymentMetrics = a} :: GetBulkDeploymentStatusResponse)

-- | The time, in ISO format, when the deployment was created.
getBulkDeploymentStatusResponse_createdAt :: Lens.Lens' GetBulkDeploymentStatusResponse (Prelude.Maybe Prelude.Text)
getBulkDeploymentStatusResponse_createdAt = Lens.lens (\GetBulkDeploymentStatusResponse' {createdAt} -> createdAt) (\s@GetBulkDeploymentStatusResponse' {} a -> s {createdAt = a} :: GetBulkDeploymentStatusResponse)

-- | The response's http status code.
getBulkDeploymentStatusResponse_httpStatus :: Lens.Lens' GetBulkDeploymentStatusResponse Prelude.Int
getBulkDeploymentStatusResponse_httpStatus = Lens.lens (\GetBulkDeploymentStatusResponse' {httpStatus} -> httpStatus) (\s@GetBulkDeploymentStatusResponse' {} a -> s {httpStatus = a} :: GetBulkDeploymentStatusResponse)

instance
  Prelude.NFData
    GetBulkDeploymentStatusResponse
  where
  rnf GetBulkDeploymentStatusResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf bulkDeploymentStatus
      `Prelude.seq` Prelude.rnf bulkDeploymentMetrics
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Evidently.UpdateProjectDataDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data storage options for this project. If you store
-- evaluation events, you an keep them and analyze them on your own. If you
-- choose not to store evaluation events, Evidently deletes them after
-- using them to produce metrics and other experiment results that you can
-- view.
--
-- You can\'t specify both @cloudWatchLogs@ and @s3Destination@ in the same
-- operation.
module Amazonka.Evidently.UpdateProjectDataDelivery
  ( -- * Creating a Request
    UpdateProjectDataDelivery (..),
    newUpdateProjectDataDelivery,

    -- * Request Lenses
    updateProjectDataDelivery_cloudWatchLogs,
    updateProjectDataDelivery_s3Destination,
    updateProjectDataDelivery_project,

    -- * Destructuring the Response
    UpdateProjectDataDeliveryResponse (..),
    newUpdateProjectDataDeliveryResponse,

    -- * Response Lenses
    updateProjectDataDeliveryResponse_httpStatus,
    updateProjectDataDeliveryResponse_project,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProjectDataDelivery' smart constructor.
data UpdateProjectDataDelivery = UpdateProjectDataDelivery'
  { -- | A structure containing the CloudWatch Logs log group where you want to
    -- store evaluation events.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsDestinationConfig,
    -- | A structure containing the S3 bucket name and bucket prefix where you
    -- want to store evaluation events.
    s3Destination :: Prelude.Maybe S3DestinationConfig,
    -- | The name or ARN of the project that you want to modify the data storage
    -- options for.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectDataDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'updateProjectDataDelivery_cloudWatchLogs' - A structure containing the CloudWatch Logs log group where you want to
-- store evaluation events.
--
-- 's3Destination', 'updateProjectDataDelivery_s3Destination' - A structure containing the S3 bucket name and bucket prefix where you
-- want to store evaluation events.
--
-- 'project', 'updateProjectDataDelivery_project' - The name or ARN of the project that you want to modify the data storage
-- options for.
newUpdateProjectDataDelivery ::
  -- | 'project'
  Prelude.Text ->
  UpdateProjectDataDelivery
newUpdateProjectDataDelivery pProject_ =
  UpdateProjectDataDelivery'
    { cloudWatchLogs =
        Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      project = pProject_
    }

-- | A structure containing the CloudWatch Logs log group where you want to
-- store evaluation events.
updateProjectDataDelivery_cloudWatchLogs :: Lens.Lens' UpdateProjectDataDelivery (Prelude.Maybe CloudWatchLogsDestinationConfig)
updateProjectDataDelivery_cloudWatchLogs = Lens.lens (\UpdateProjectDataDelivery' {cloudWatchLogs} -> cloudWatchLogs) (\s@UpdateProjectDataDelivery' {} a -> s {cloudWatchLogs = a} :: UpdateProjectDataDelivery)

-- | A structure containing the S3 bucket name and bucket prefix where you
-- want to store evaluation events.
updateProjectDataDelivery_s3Destination :: Lens.Lens' UpdateProjectDataDelivery (Prelude.Maybe S3DestinationConfig)
updateProjectDataDelivery_s3Destination = Lens.lens (\UpdateProjectDataDelivery' {s3Destination} -> s3Destination) (\s@UpdateProjectDataDelivery' {} a -> s {s3Destination = a} :: UpdateProjectDataDelivery)

-- | The name or ARN of the project that you want to modify the data storage
-- options for.
updateProjectDataDelivery_project :: Lens.Lens' UpdateProjectDataDelivery Prelude.Text
updateProjectDataDelivery_project = Lens.lens (\UpdateProjectDataDelivery' {project} -> project) (\s@UpdateProjectDataDelivery' {} a -> s {project = a} :: UpdateProjectDataDelivery)

instance Core.AWSRequest UpdateProjectDataDelivery where
  type
    AWSResponse UpdateProjectDataDelivery =
      UpdateProjectDataDeliveryResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectDataDeliveryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "project")
      )

instance Prelude.Hashable UpdateProjectDataDelivery where
  hashWithSalt _salt UpdateProjectDataDelivery' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` s3Destination
      `Prelude.hashWithSalt` project

instance Prelude.NFData UpdateProjectDataDelivery where
  rnf UpdateProjectDataDelivery' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders UpdateProjectDataDelivery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProjectDataDelivery where
  toJSON UpdateProjectDataDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Data..=)
              Prelude.<$> cloudWatchLogs,
            ("s3Destination" Data..=) Prelude.<$> s3Destination
          ]
      )

instance Data.ToPath UpdateProjectDataDelivery where
  toPath UpdateProjectDataDelivery' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/data-delivery"]

instance Data.ToQuery UpdateProjectDataDelivery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectDataDeliveryResponse' smart constructor.
data UpdateProjectDataDeliveryResponse = UpdateProjectDataDeliveryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the project that you updated.
    project :: Project
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectDataDeliveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProjectDataDeliveryResponse_httpStatus' - The response's http status code.
--
-- 'project', 'updateProjectDataDeliveryResponse_project' - A structure containing details about the project that you updated.
newUpdateProjectDataDeliveryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'project'
  Project ->
  UpdateProjectDataDeliveryResponse
newUpdateProjectDataDeliveryResponse
  pHttpStatus_
  pProject_ =
    UpdateProjectDataDeliveryResponse'
      { httpStatus =
          pHttpStatus_,
        project = pProject_
      }

-- | The response's http status code.
updateProjectDataDeliveryResponse_httpStatus :: Lens.Lens' UpdateProjectDataDeliveryResponse Prelude.Int
updateProjectDataDeliveryResponse_httpStatus = Lens.lens (\UpdateProjectDataDeliveryResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectDataDeliveryResponse' {} a -> s {httpStatus = a} :: UpdateProjectDataDeliveryResponse)

-- | A structure containing details about the project that you updated.
updateProjectDataDeliveryResponse_project :: Lens.Lens' UpdateProjectDataDeliveryResponse Project
updateProjectDataDeliveryResponse_project = Lens.lens (\UpdateProjectDataDeliveryResponse' {project} -> project) (\s@UpdateProjectDataDeliveryResponse' {} a -> s {project = a} :: UpdateProjectDataDeliveryResponse)

instance
  Prelude.NFData
    UpdateProjectDataDeliveryResponse
  where
  rnf UpdateProjectDataDeliveryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf project

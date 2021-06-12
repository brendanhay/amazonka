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
-- Module      : Network.AWS.Greengrass.StopBulkDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of a bulk deployment. This action returns a status
-- of \'\'Stopping\'\' until the deployment is stopped. You cannot start a
-- new bulk deployment while a previous deployment is in the
-- \'\'Stopping\'\' state. This action doesn\'t rollback completed
-- deployments or cancel pending deployments.
module Network.AWS.Greengrass.StopBulkDeployment
  ( -- * Creating a Request
    StopBulkDeployment (..),
    newStopBulkDeployment,

    -- * Request Lenses
    stopBulkDeployment_bulkDeploymentId,

    -- * Destructuring the Response
    StopBulkDeploymentResponse (..),
    newStopBulkDeploymentResponse,

    -- * Response Lenses
    stopBulkDeploymentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopBulkDeployment' smart constructor.
data StopBulkDeployment = StopBulkDeployment'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopBulkDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeploymentId', 'stopBulkDeployment_bulkDeploymentId' - The ID of the bulk deployment.
newStopBulkDeployment ::
  -- | 'bulkDeploymentId'
  Core.Text ->
  StopBulkDeployment
newStopBulkDeployment pBulkDeploymentId_ =
  StopBulkDeployment'
    { bulkDeploymentId =
        pBulkDeploymentId_
    }

-- | The ID of the bulk deployment.
stopBulkDeployment_bulkDeploymentId :: Lens.Lens' StopBulkDeployment Core.Text
stopBulkDeployment_bulkDeploymentId = Lens.lens (\StopBulkDeployment' {bulkDeploymentId} -> bulkDeploymentId) (\s@StopBulkDeployment' {} a -> s {bulkDeploymentId = a} :: StopBulkDeployment)

instance Core.AWSRequest StopBulkDeployment where
  type
    AWSResponse StopBulkDeployment =
      StopBulkDeploymentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopBulkDeploymentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopBulkDeployment

instance Core.NFData StopBulkDeployment

instance Core.ToHeaders StopBulkDeployment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopBulkDeployment where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath StopBulkDeployment where
  toPath StopBulkDeployment' {..} =
    Core.mconcat
      [ "/greengrass/bulk/deployments/",
        Core.toBS bulkDeploymentId,
        "/$stop"
      ]

instance Core.ToQuery StopBulkDeployment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopBulkDeploymentResponse' smart constructor.
data StopBulkDeploymentResponse = StopBulkDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopBulkDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopBulkDeploymentResponse_httpStatus' - The response's http status code.
newStopBulkDeploymentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopBulkDeploymentResponse
newStopBulkDeploymentResponse pHttpStatus_ =
  StopBulkDeploymentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopBulkDeploymentResponse_httpStatus :: Lens.Lens' StopBulkDeploymentResponse Core.Int
stopBulkDeploymentResponse_httpStatus = Lens.lens (\StopBulkDeploymentResponse' {httpStatus} -> httpStatus) (\s@StopBulkDeploymentResponse' {} a -> s {httpStatus = a} :: StopBulkDeploymentResponse)

instance Core.NFData StopBulkDeploymentResponse

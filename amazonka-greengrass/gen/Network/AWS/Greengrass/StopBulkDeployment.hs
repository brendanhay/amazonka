{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopBulkDeployment' smart constructor.
data StopBulkDeployment = StopBulkDeployment'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopBulkDeployment
newStopBulkDeployment pBulkDeploymentId_ =
  StopBulkDeployment'
    { bulkDeploymentId =
        pBulkDeploymentId_
    }

-- | The ID of the bulk deployment.
stopBulkDeployment_bulkDeploymentId :: Lens.Lens' StopBulkDeployment Prelude.Text
stopBulkDeployment_bulkDeploymentId = Lens.lens (\StopBulkDeployment' {bulkDeploymentId} -> bulkDeploymentId) (\s@StopBulkDeployment' {} a -> s {bulkDeploymentId = a} :: StopBulkDeployment)

instance Prelude.AWSRequest StopBulkDeployment where
  type
    Rs StopBulkDeployment =
      StopBulkDeploymentResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopBulkDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopBulkDeployment

instance Prelude.NFData StopBulkDeployment

instance Prelude.ToHeaders StopBulkDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopBulkDeployment where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath StopBulkDeployment where
  toPath StopBulkDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/bulk/deployments/",
        Prelude.toBS bulkDeploymentId,
        "/$stop"
      ]

instance Prelude.ToQuery StopBulkDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBulkDeploymentResponse' smart constructor.
data StopBulkDeploymentResponse = StopBulkDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopBulkDeploymentResponse
newStopBulkDeploymentResponse pHttpStatus_ =
  StopBulkDeploymentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopBulkDeploymentResponse_httpStatus :: Lens.Lens' StopBulkDeploymentResponse Prelude.Int
stopBulkDeploymentResponse_httpStatus = Lens.lens (\StopBulkDeploymentResponse' {httpStatus} -> httpStatus) (\s@StopBulkDeploymentResponse' {} a -> s {httpStatus = a} :: StopBulkDeploymentResponse)

instance Prelude.NFData StopBulkDeploymentResponse

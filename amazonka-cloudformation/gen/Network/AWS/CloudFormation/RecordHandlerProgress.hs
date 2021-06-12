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
-- Module      : Network.AWS.CloudFormation.RecordHandlerProgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reports progress of a resource handler to CloudFormation.
--
-- Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- Do not use this API in your code.
module Network.AWS.CloudFormation.RecordHandlerProgress
  ( -- * Creating a Request
    RecordHandlerProgress (..),
    newRecordHandlerProgress,

    -- * Request Lenses
    recordHandlerProgress_statusMessage,
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_errorCode,
    recordHandlerProgress_bearerToken,
    recordHandlerProgress_operationStatus,

    -- * Destructuring the Response
    RecordHandlerProgressResponse (..),
    newRecordHandlerProgressResponse,

    -- * Response Lenses
    recordHandlerProgressResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRecordHandlerProgress' smart constructor.
data RecordHandlerProgress = RecordHandlerProgress'
  { -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    statusMessage :: Core.Maybe Core.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    resourceModel :: Core.Maybe Core.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    currentOperationStatus :: Core.Maybe OperationStatus,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    errorCode :: Core.Maybe HandlerErrorCode,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    bearerToken :: Core.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    operationStatus :: OperationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordHandlerProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'recordHandlerProgress_statusMessage' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'resourceModel', 'recordHandlerProgress_resourceModel' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'clientRequestToken', 'recordHandlerProgress_clientRequestToken' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'currentOperationStatus', 'recordHandlerProgress_currentOperationStatus' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'errorCode', 'recordHandlerProgress_errorCode' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'bearerToken', 'recordHandlerProgress_bearerToken' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'operationStatus', 'recordHandlerProgress_operationStatus' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
newRecordHandlerProgress ::
  -- | 'bearerToken'
  Core.Text ->
  -- | 'operationStatus'
  OperationStatus ->
  RecordHandlerProgress
newRecordHandlerProgress
  pBearerToken_
  pOperationStatus_ =
    RecordHandlerProgress'
      { statusMessage =
          Core.Nothing,
        resourceModel = Core.Nothing,
        clientRequestToken = Core.Nothing,
        currentOperationStatus = Core.Nothing,
        errorCode = Core.Nothing,
        bearerToken = pBearerToken_,
        operationStatus = pOperationStatus_
      }

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_statusMessage :: Lens.Lens' RecordHandlerProgress (Core.Maybe Core.Text)
recordHandlerProgress_statusMessage = Lens.lens (\RecordHandlerProgress' {statusMessage} -> statusMessage) (\s@RecordHandlerProgress' {} a -> s {statusMessage = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_resourceModel :: Lens.Lens' RecordHandlerProgress (Core.Maybe Core.Text)
recordHandlerProgress_resourceModel = Lens.lens (\RecordHandlerProgress' {resourceModel} -> resourceModel) (\s@RecordHandlerProgress' {} a -> s {resourceModel = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_clientRequestToken :: Lens.Lens' RecordHandlerProgress (Core.Maybe Core.Text)
recordHandlerProgress_clientRequestToken = Lens.lens (\RecordHandlerProgress' {clientRequestToken} -> clientRequestToken) (\s@RecordHandlerProgress' {} a -> s {clientRequestToken = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_currentOperationStatus :: Lens.Lens' RecordHandlerProgress (Core.Maybe OperationStatus)
recordHandlerProgress_currentOperationStatus = Lens.lens (\RecordHandlerProgress' {currentOperationStatus} -> currentOperationStatus) (\s@RecordHandlerProgress' {} a -> s {currentOperationStatus = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_errorCode :: Lens.Lens' RecordHandlerProgress (Core.Maybe HandlerErrorCode)
recordHandlerProgress_errorCode = Lens.lens (\RecordHandlerProgress' {errorCode} -> errorCode) (\s@RecordHandlerProgress' {} a -> s {errorCode = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_bearerToken :: Lens.Lens' RecordHandlerProgress Core.Text
recordHandlerProgress_bearerToken = Lens.lens (\RecordHandlerProgress' {bearerToken} -> bearerToken) (\s@RecordHandlerProgress' {} a -> s {bearerToken = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_operationStatus :: Lens.Lens' RecordHandlerProgress OperationStatus
recordHandlerProgress_operationStatus = Lens.lens (\RecordHandlerProgress' {operationStatus} -> operationStatus) (\s@RecordHandlerProgress' {} a -> s {operationStatus = a} :: RecordHandlerProgress)

instance Core.AWSRequest RecordHandlerProgress where
  type
    AWSResponse RecordHandlerProgress =
      RecordHandlerProgressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RecordHandlerProgressResult"
      ( \s h x ->
          RecordHandlerProgressResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RecordHandlerProgress

instance Core.NFData RecordHandlerProgress

instance Core.ToHeaders RecordHandlerProgress where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RecordHandlerProgress where
  toPath = Core.const "/"

instance Core.ToQuery RecordHandlerProgress where
  toQuery RecordHandlerProgress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RecordHandlerProgress" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "StatusMessage" Core.=: statusMessage,
        "ResourceModel" Core.=: resourceModel,
        "ClientRequestToken" Core.=: clientRequestToken,
        "CurrentOperationStatus"
          Core.=: currentOperationStatus,
        "ErrorCode" Core.=: errorCode,
        "BearerToken" Core.=: bearerToken,
        "OperationStatus" Core.=: operationStatus
      ]

-- | /See:/ 'newRecordHandlerProgressResponse' smart constructor.
data RecordHandlerProgressResponse = RecordHandlerProgressResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordHandlerProgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'recordHandlerProgressResponse_httpStatus' - The response's http status code.
newRecordHandlerProgressResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RecordHandlerProgressResponse
newRecordHandlerProgressResponse pHttpStatus_ =
  RecordHandlerProgressResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
recordHandlerProgressResponse_httpStatus :: Lens.Lens' RecordHandlerProgressResponse Core.Int
recordHandlerProgressResponse_httpStatus = Lens.lens (\RecordHandlerProgressResponse' {httpStatus} -> httpStatus) (\s@RecordHandlerProgressResponse' {} a -> s {httpStatus = a} :: RecordHandlerProgressResponse)

instance Core.NFData RecordHandlerProgressResponse

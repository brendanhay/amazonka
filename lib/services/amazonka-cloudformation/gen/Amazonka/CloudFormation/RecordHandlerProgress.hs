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
-- Module      : Amazonka.CloudFormation.RecordHandlerProgress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reports progress of a resource handler to CloudFormation.
--
-- Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- Don\'t use this API in your code.
module Amazonka.CloudFormation.RecordHandlerProgress
  ( -- * Creating a Request
    RecordHandlerProgress (..),
    newRecordHandlerProgress,

    -- * Request Lenses
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_errorCode,
    recordHandlerProgress_statusMessage,
    recordHandlerProgress_bearerToken,
    recordHandlerProgress_operationStatus,

    -- * Destructuring the Response
    RecordHandlerProgressResponse (..),
    newRecordHandlerProgressResponse,

    -- * Response Lenses
    recordHandlerProgressResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRecordHandlerProgress' smart constructor.
data RecordHandlerProgress = RecordHandlerProgress'
  { -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    resourceModel :: Prelude.Maybe Prelude.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    currentOperationStatus :: Prelude.Maybe OperationStatus,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    errorCode :: Prelude.Maybe HandlerErrorCode,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    bearerToken :: Prelude.Text,
    -- | Reserved for use by the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
    operationStatus :: OperationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordHandlerProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceModel', 'recordHandlerProgress_resourceModel' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'currentOperationStatus', 'recordHandlerProgress_currentOperationStatus' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'clientRequestToken', 'recordHandlerProgress_clientRequestToken' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'errorCode', 'recordHandlerProgress_errorCode' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'statusMessage', 'recordHandlerProgress_statusMessage' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'bearerToken', 'recordHandlerProgress_bearerToken' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
--
-- 'operationStatus', 'recordHandlerProgress_operationStatus' - Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
newRecordHandlerProgress ::
  -- | 'bearerToken'
  Prelude.Text ->
  -- | 'operationStatus'
  OperationStatus ->
  RecordHandlerProgress
newRecordHandlerProgress
  pBearerToken_
  pOperationStatus_ =
    RecordHandlerProgress'
      { resourceModel =
          Prelude.Nothing,
        currentOperationStatus = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        errorCode = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        bearerToken = pBearerToken_,
        operationStatus = pOperationStatus_
      }

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_resourceModel :: Lens.Lens' RecordHandlerProgress (Prelude.Maybe Prelude.Text)
recordHandlerProgress_resourceModel = Lens.lens (\RecordHandlerProgress' {resourceModel} -> resourceModel) (\s@RecordHandlerProgress' {} a -> s {resourceModel = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_currentOperationStatus :: Lens.Lens' RecordHandlerProgress (Prelude.Maybe OperationStatus)
recordHandlerProgress_currentOperationStatus = Lens.lens (\RecordHandlerProgress' {currentOperationStatus} -> currentOperationStatus) (\s@RecordHandlerProgress' {} a -> s {currentOperationStatus = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_clientRequestToken :: Lens.Lens' RecordHandlerProgress (Prelude.Maybe Prelude.Text)
recordHandlerProgress_clientRequestToken = Lens.lens (\RecordHandlerProgress' {clientRequestToken} -> clientRequestToken) (\s@RecordHandlerProgress' {} a -> s {clientRequestToken = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_errorCode :: Lens.Lens' RecordHandlerProgress (Prelude.Maybe HandlerErrorCode)
recordHandlerProgress_errorCode = Lens.lens (\RecordHandlerProgress' {errorCode} -> errorCode) (\s@RecordHandlerProgress' {} a -> s {errorCode = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_statusMessage :: Lens.Lens' RecordHandlerProgress (Prelude.Maybe Prelude.Text)
recordHandlerProgress_statusMessage = Lens.lens (\RecordHandlerProgress' {statusMessage} -> statusMessage) (\s@RecordHandlerProgress' {} a -> s {statusMessage = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_bearerToken :: Lens.Lens' RecordHandlerProgress Prelude.Text
recordHandlerProgress_bearerToken = Lens.lens (\RecordHandlerProgress' {bearerToken} -> bearerToken) (\s@RecordHandlerProgress' {} a -> s {bearerToken = a} :: RecordHandlerProgress)

-- | Reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
recordHandlerProgress_operationStatus :: Lens.Lens' RecordHandlerProgress OperationStatus
recordHandlerProgress_operationStatus = Lens.lens (\RecordHandlerProgress' {operationStatus} -> operationStatus) (\s@RecordHandlerProgress' {} a -> s {operationStatus = a} :: RecordHandlerProgress)

instance Core.AWSRequest RecordHandlerProgress where
  type
    AWSResponse RecordHandlerProgress =
      RecordHandlerProgressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RecordHandlerProgressResult"
      ( \s h x ->
          RecordHandlerProgressResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RecordHandlerProgress where
  hashWithSalt _salt RecordHandlerProgress' {..} =
    _salt `Prelude.hashWithSalt` resourceModel
      `Prelude.hashWithSalt` currentOperationStatus
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` bearerToken
      `Prelude.hashWithSalt` operationStatus

instance Prelude.NFData RecordHandlerProgress where
  rnf RecordHandlerProgress' {..} =
    Prelude.rnf resourceModel
      `Prelude.seq` Prelude.rnf currentOperationStatus
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf bearerToken
      `Prelude.seq` Prelude.rnf operationStatus

instance Data.ToHeaders RecordHandlerProgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RecordHandlerProgress where
  toPath = Prelude.const "/"

instance Data.ToQuery RecordHandlerProgress where
  toQuery RecordHandlerProgress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RecordHandlerProgress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ResourceModel" Data.=: resourceModel,
        "CurrentOperationStatus"
          Data.=: currentOperationStatus,
        "ClientRequestToken" Data.=: clientRequestToken,
        "ErrorCode" Data.=: errorCode,
        "StatusMessage" Data.=: statusMessage,
        "BearerToken" Data.=: bearerToken,
        "OperationStatus" Data.=: operationStatus
      ]

-- | /See:/ 'newRecordHandlerProgressResponse' smart constructor.
data RecordHandlerProgressResponse = RecordHandlerProgressResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RecordHandlerProgressResponse
newRecordHandlerProgressResponse pHttpStatus_ =
  RecordHandlerProgressResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
recordHandlerProgressResponse_httpStatus :: Lens.Lens' RecordHandlerProgressResponse Prelude.Int
recordHandlerProgressResponse_httpStatus = Lens.lens (\RecordHandlerProgressResponse' {httpStatus} -> httpStatus) (\s@RecordHandlerProgressResponse' {} a -> s {httpStatus = a} :: RecordHandlerProgressResponse)

instance Prelude.NFData RecordHandlerProgressResponse where
  rnf RecordHandlerProgressResponse' {..} =
    Prelude.rnf httpStatus

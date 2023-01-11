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
-- Module      : Amazonka.SSOAdmin.DescribeAccountAssignmentCreationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the assignment creation request.
module Amazonka.SSOAdmin.DescribeAccountAssignmentCreationStatus
  ( -- * Creating a Request
    DescribeAccountAssignmentCreationStatus (..),
    newDescribeAccountAssignmentCreationStatus,

    -- * Request Lenses
    describeAccountAssignmentCreationStatus_instanceArn,
    describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId,

    -- * Destructuring the Response
    DescribeAccountAssignmentCreationStatusResponse (..),
    newDescribeAccountAssignmentCreationStatusResponse,

    -- * Response Lenses
    describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus,
    describeAccountAssignmentCreationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDescribeAccountAssignmentCreationStatus' smart constructor.
data DescribeAccountAssignmentCreationStatus = DescribeAccountAssignmentCreationStatus'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The identifier that is used to track the request operation progress.
    accountAssignmentCreationRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAssignmentCreationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'describeAccountAssignmentCreationStatus_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'accountAssignmentCreationRequestId', 'describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId' - The identifier that is used to track the request operation progress.
newDescribeAccountAssignmentCreationStatus ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'accountAssignmentCreationRequestId'
  Prelude.Text ->
  DescribeAccountAssignmentCreationStatus
newDescribeAccountAssignmentCreationStatus
  pInstanceArn_
  pAccountAssignmentCreationRequestId_ =
    DescribeAccountAssignmentCreationStatus'
      { instanceArn =
          pInstanceArn_,
        accountAssignmentCreationRequestId =
          pAccountAssignmentCreationRequestId_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
describeAccountAssignmentCreationStatus_instanceArn :: Lens.Lens' DescribeAccountAssignmentCreationStatus Prelude.Text
describeAccountAssignmentCreationStatus_instanceArn = Lens.lens (\DescribeAccountAssignmentCreationStatus' {instanceArn} -> instanceArn) (\s@DescribeAccountAssignmentCreationStatus' {} a -> s {instanceArn = a} :: DescribeAccountAssignmentCreationStatus)

-- | The identifier that is used to track the request operation progress.
describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId :: Lens.Lens' DescribeAccountAssignmentCreationStatus Prelude.Text
describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId = Lens.lens (\DescribeAccountAssignmentCreationStatus' {accountAssignmentCreationRequestId} -> accountAssignmentCreationRequestId) (\s@DescribeAccountAssignmentCreationStatus' {} a -> s {accountAssignmentCreationRequestId = a} :: DescribeAccountAssignmentCreationStatus)

instance
  Core.AWSRequest
    DescribeAccountAssignmentCreationStatus
  where
  type
    AWSResponse
      DescribeAccountAssignmentCreationStatus =
      DescribeAccountAssignmentCreationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAssignmentCreationStatusResponse'
            Prelude.<$> (x Data..?> "AccountAssignmentCreationStatus")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAccountAssignmentCreationStatus
  where
  hashWithSalt
    _salt
    DescribeAccountAssignmentCreationStatus' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` accountAssignmentCreationRequestId

instance
  Prelude.NFData
    DescribeAccountAssignmentCreationStatus
  where
  rnf DescribeAccountAssignmentCreationStatus' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf accountAssignmentCreationRequestId

instance
  Data.ToHeaders
    DescribeAccountAssignmentCreationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.DescribeAccountAssignmentCreationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeAccountAssignmentCreationStatus
  where
  toJSON DescribeAccountAssignmentCreationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ( "AccountAssignmentCreationRequestId"
                  Data..= accountAssignmentCreationRequestId
              )
          ]
      )

instance
  Data.ToPath
    DescribeAccountAssignmentCreationStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAccountAssignmentCreationStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountAssignmentCreationStatusResponse' smart constructor.
data DescribeAccountAssignmentCreationStatusResponse = DescribeAccountAssignmentCreationStatusResponse'
  { -- | The status object for the account assignment creation operation.
    accountAssignmentCreationStatus :: Prelude.Maybe AccountAssignmentOperationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAssignmentCreationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAssignmentCreationStatus', 'describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus' - The status object for the account assignment creation operation.
--
-- 'httpStatus', 'describeAccountAssignmentCreationStatusResponse_httpStatus' - The response's http status code.
newDescribeAccountAssignmentCreationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAssignmentCreationStatusResponse
newDescribeAccountAssignmentCreationStatusResponse
  pHttpStatus_ =
    DescribeAccountAssignmentCreationStatusResponse'
      { accountAssignmentCreationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status object for the account assignment creation operation.
describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus :: Lens.Lens' DescribeAccountAssignmentCreationStatusResponse (Prelude.Maybe AccountAssignmentOperationStatus)
describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus = Lens.lens (\DescribeAccountAssignmentCreationStatusResponse' {accountAssignmentCreationStatus} -> accountAssignmentCreationStatus) (\s@DescribeAccountAssignmentCreationStatusResponse' {} a -> s {accountAssignmentCreationStatus = a} :: DescribeAccountAssignmentCreationStatusResponse)

-- | The response's http status code.
describeAccountAssignmentCreationStatusResponse_httpStatus :: Lens.Lens' DescribeAccountAssignmentCreationStatusResponse Prelude.Int
describeAccountAssignmentCreationStatusResponse_httpStatus = Lens.lens (\DescribeAccountAssignmentCreationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAssignmentCreationStatusResponse' {} a -> s {httpStatus = a} :: DescribeAccountAssignmentCreationStatusResponse)

instance
  Prelude.NFData
    DescribeAccountAssignmentCreationStatusResponse
  where
  rnf
    DescribeAccountAssignmentCreationStatusResponse' {..} =
      Prelude.rnf accountAssignmentCreationStatus
        `Prelude.seq` Prelude.rnf httpStatus

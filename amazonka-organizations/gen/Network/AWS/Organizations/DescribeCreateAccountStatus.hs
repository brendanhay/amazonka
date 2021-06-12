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
-- Module      : Network.AWS.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of an asynchronous request to create an
-- account.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
module Network.AWS.Organizations.DescribeCreateAccountStatus
  ( -- * Creating a Request
    DescribeCreateAccountStatus (..),
    newDescribeCreateAccountStatus,

    -- * Request Lenses
    describeCreateAccountStatus_createAccountRequestId,

    -- * Destructuring the Response
    DescribeCreateAccountStatusResponse (..),
    newDescribeCreateAccountStatusResponse,

    -- * Response Lenses
    describeCreateAccountStatusResponse_createAccountStatus,
    describeCreateAccountStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCreateAccountStatus' smart constructor.
data DescribeCreateAccountStatus = DescribeCreateAccountStatus'
  { -- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@
    -- request. You can get the value from the @CreateAccountStatus.Id@
    -- response in an earlier CreateAccount request, or from the
    -- ListCreateAccountStatus operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
    -- request ID string requires \"car-\" followed by from 8 to 32 lowercase
    -- letters or digits.
    createAccountRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCreateAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createAccountRequestId', 'describeCreateAccountStatus_createAccountRequestId' - Specifies the @Id@ value that uniquely identifies the @CreateAccount@
-- request. You can get the value from the @CreateAccountStatus.Id@
-- response in an earlier CreateAccount request, or from the
-- ListCreateAccountStatus operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
newDescribeCreateAccountStatus ::
  -- | 'createAccountRequestId'
  Core.Text ->
  DescribeCreateAccountStatus
newDescribeCreateAccountStatus
  pCreateAccountRequestId_ =
    DescribeCreateAccountStatus'
      { createAccountRequestId =
          pCreateAccountRequestId_
      }

-- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@
-- request. You can get the value from the @CreateAccountStatus.Id@
-- response in an earlier CreateAccount request, or from the
-- ListCreateAccountStatus operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
describeCreateAccountStatus_createAccountRequestId :: Lens.Lens' DescribeCreateAccountStatus Core.Text
describeCreateAccountStatus_createAccountRequestId = Lens.lens (\DescribeCreateAccountStatus' {createAccountRequestId} -> createAccountRequestId) (\s@DescribeCreateAccountStatus' {} a -> s {createAccountRequestId = a} :: DescribeCreateAccountStatus)

instance Core.AWSRequest DescribeCreateAccountStatus where
  type
    AWSResponse DescribeCreateAccountStatus =
      DescribeCreateAccountStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCreateAccountStatusResponse'
            Core.<$> (x Core..?> "CreateAccountStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCreateAccountStatus

instance Core.NFData DescribeCreateAccountStatus

instance Core.ToHeaders DescribeCreateAccountStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeCreateAccountStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCreateAccountStatus where
  toJSON DescribeCreateAccountStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "CreateAccountRequestId"
                  Core..= createAccountRequestId
              )
          ]
      )

instance Core.ToPath DescribeCreateAccountStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCreateAccountStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { -- | A structure that contains the current status of an account creation
    -- request.
    createAccountStatus :: Core.Maybe CreateAccountStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCreateAccountStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createAccountStatus', 'describeCreateAccountStatusResponse_createAccountStatus' - A structure that contains the current status of an account creation
-- request.
--
-- 'httpStatus', 'describeCreateAccountStatusResponse_httpStatus' - The response's http status code.
newDescribeCreateAccountStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCreateAccountStatusResponse
newDescribeCreateAccountStatusResponse pHttpStatus_ =
  DescribeCreateAccountStatusResponse'
    { createAccountStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the current status of an account creation
-- request.
describeCreateAccountStatusResponse_createAccountStatus :: Lens.Lens' DescribeCreateAccountStatusResponse (Core.Maybe CreateAccountStatus)
describeCreateAccountStatusResponse_createAccountStatus = Lens.lens (\DescribeCreateAccountStatusResponse' {createAccountStatus} -> createAccountStatus) (\s@DescribeCreateAccountStatusResponse' {} a -> s {createAccountStatus = a} :: DescribeCreateAccountStatusResponse)

-- | The response's http status code.
describeCreateAccountStatusResponse_httpStatus :: Lens.Lens' DescribeCreateAccountStatusResponse Core.Int
describeCreateAccountStatusResponse_httpStatus = Lens.lens (\DescribeCreateAccountStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeCreateAccountStatusResponse' {} a -> s {httpStatus = a} :: DescribeCreateAccountStatusResponse)

instance
  Core.NFData
    DescribeCreateAccountStatusResponse

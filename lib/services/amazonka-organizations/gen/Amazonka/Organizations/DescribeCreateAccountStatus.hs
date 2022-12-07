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
-- Module      : Amazonka.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Amazon Web Services service.
module Amazonka.Organizations.DescribeCreateAccountStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    createAccountRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
describeCreateAccountStatus_createAccountRequestId :: Lens.Lens' DescribeCreateAccountStatus Prelude.Text
describeCreateAccountStatus_createAccountRequestId = Lens.lens (\DescribeCreateAccountStatus' {createAccountRequestId} -> createAccountRequestId) (\s@DescribeCreateAccountStatus' {} a -> s {createAccountRequestId = a} :: DescribeCreateAccountStatus)

instance Core.AWSRequest DescribeCreateAccountStatus where
  type
    AWSResponse DescribeCreateAccountStatus =
      DescribeCreateAccountStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCreateAccountStatusResponse'
            Prelude.<$> (x Data..?> "CreateAccountStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCreateAccountStatus where
  hashWithSalt _salt DescribeCreateAccountStatus' {..} =
    _salt `Prelude.hashWithSalt` createAccountRequestId

instance Prelude.NFData DescribeCreateAccountStatus where
  rnf DescribeCreateAccountStatus' {..} =
    Prelude.rnf createAccountRequestId

instance Data.ToHeaders DescribeCreateAccountStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DescribeCreateAccountStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCreateAccountStatus where
  toJSON DescribeCreateAccountStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CreateAccountRequestId"
                  Data..= createAccountRequestId
              )
          ]
      )

instance Data.ToPath DescribeCreateAccountStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCreateAccountStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { -- | A structure that contains the current status of an account creation
    -- request.
    createAccountStatus :: Prelude.Maybe CreateAccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeCreateAccountStatusResponse
newDescribeCreateAccountStatusResponse pHttpStatus_ =
  DescribeCreateAccountStatusResponse'
    { createAccountStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the current status of an account creation
-- request.
describeCreateAccountStatusResponse_createAccountStatus :: Lens.Lens' DescribeCreateAccountStatusResponse (Prelude.Maybe CreateAccountStatus)
describeCreateAccountStatusResponse_createAccountStatus = Lens.lens (\DescribeCreateAccountStatusResponse' {createAccountStatus} -> createAccountStatus) (\s@DescribeCreateAccountStatusResponse' {} a -> s {createAccountStatus = a} :: DescribeCreateAccountStatusResponse)

-- | The response's http status code.
describeCreateAccountStatusResponse_httpStatus :: Lens.Lens' DescribeCreateAccountStatusResponse Prelude.Int
describeCreateAccountStatusResponse_httpStatus = Lens.lens (\DescribeCreateAccountStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeCreateAccountStatusResponse' {} a -> s {httpStatus = a} :: DescribeCreateAccountStatusResponse)

instance
  Prelude.NFData
    DescribeCreateAccountStatusResponse
  where
  rnf DescribeCreateAccountStatusResponse' {..} =
    Prelude.rnf createAccountStatus
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty
-- delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    EnableOrganizationAdminAccount (..),
    newEnableOrganizationAdminAccount,

    -- * Request Lenses
    enableOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    EnableOrganizationAdminAccountResponse (..),
    newEnableOrganizationAdminAccountResponse,

    -- * Response Lenses
    enableOrganizationAdminAccountResponse_httpStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableOrganizationAdminAccount' smart constructor.
data EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organization account to be enabled as a
    -- GuardDuty delegated administrator.
    adminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'enableOrganizationAdminAccount_adminAccountId' - The AWS Account ID for the organization account to be enabled as a
-- GuardDuty delegated administrator.
newEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  EnableOrganizationAdminAccount
newEnableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organization account to be enabled as a
-- GuardDuty delegated administrator.
enableOrganizationAdminAccount_adminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Prelude.Text
enableOrganizationAdminAccount_adminAccountId = Lens.lens (\EnableOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@EnableOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: EnableOrganizationAdminAccount)

instance
  Prelude.AWSRequest
    EnableOrganizationAdminAccount
  where
  type
    Rs EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableOrganizationAdminAccount

instance
  Prelude.NFData
    EnableOrganizationAdminAccount

instance
  Prelude.ToHeaders
    EnableOrganizationAdminAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    EnableOrganizationAdminAccount
  where
  toJSON EnableOrganizationAdminAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("adminAccountId" Prelude..= adminAccountId)
          ]
      )

instance
  Prelude.ToPath
    EnableOrganizationAdminAccount
  where
  toPath = Prelude.const "/admin/enable"

instance
  Prelude.ToQuery
    EnableOrganizationAdminAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableOrganizationAdminAccountResponse' smart constructor.
data EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newEnableOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableOrganizationAdminAccountResponse
newEnableOrganizationAdminAccountResponse
  pHttpStatus_ =
    EnableOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Prelude.Int
enableOrganizationAdminAccountResponse_httpStatus = Lens.lens (\EnableOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@EnableOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: EnableOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    EnableOrganizationAdminAccountResponse

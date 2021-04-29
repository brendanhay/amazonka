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
-- Module      : Network.AWS.Greengrass.GetServiceRoleForAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the service role that is attached to your account.
module Network.AWS.Greengrass.GetServiceRoleForAccount
  ( -- * Creating a Request
    GetServiceRoleForAccount (..),
    newGetServiceRoleForAccount,

    -- * Destructuring the Response
    GetServiceRoleForAccountResponse (..),
    newGetServiceRoleForAccountResponse,

    -- * Response Lenses
    getServiceRoleForAccountResponse_roleArn,
    getServiceRoleForAccountResponse_associatedAt,
    getServiceRoleForAccountResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetServiceRoleForAccount' smart constructor.
data GetServiceRoleForAccount = GetServiceRoleForAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetServiceRoleForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetServiceRoleForAccount ::
  GetServiceRoleForAccount
newGetServiceRoleForAccount =
  GetServiceRoleForAccount'

instance Prelude.AWSRequest GetServiceRoleForAccount where
  type
    Rs GetServiceRoleForAccount =
      GetServiceRoleForAccountResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceRoleForAccountResponse'
            Prelude.<$> (x Prelude..?> "RoleArn")
            Prelude.<*> (x Prelude..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceRoleForAccount

instance Prelude.NFData GetServiceRoleForAccount

instance Prelude.ToHeaders GetServiceRoleForAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetServiceRoleForAccount where
  toPath = Prelude.const "/greengrass/servicerole"

instance Prelude.ToQuery GetServiceRoleForAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceRoleForAccountResponse' smart constructor.
data GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse'
  { -- | The ARN of the role which is associated with the account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the service role was associated with the account.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetServiceRoleForAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getServiceRoleForAccountResponse_roleArn' - The ARN of the role which is associated with the account.
--
-- 'associatedAt', 'getServiceRoleForAccountResponse_associatedAt' - The time when the service role was associated with the account.
--
-- 'httpStatus', 'getServiceRoleForAccountResponse_httpStatus' - The response's http status code.
newGetServiceRoleForAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceRoleForAccountResponse
newGetServiceRoleForAccountResponse pHttpStatus_ =
  GetServiceRoleForAccountResponse'
    { roleArn =
        Prelude.Nothing,
      associatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the role which is associated with the account.
getServiceRoleForAccountResponse_roleArn :: Lens.Lens' GetServiceRoleForAccountResponse (Prelude.Maybe Prelude.Text)
getServiceRoleForAccountResponse_roleArn = Lens.lens (\GetServiceRoleForAccountResponse' {roleArn} -> roleArn) (\s@GetServiceRoleForAccountResponse' {} a -> s {roleArn = a} :: GetServiceRoleForAccountResponse)

-- | The time when the service role was associated with the account.
getServiceRoleForAccountResponse_associatedAt :: Lens.Lens' GetServiceRoleForAccountResponse (Prelude.Maybe Prelude.Text)
getServiceRoleForAccountResponse_associatedAt = Lens.lens (\GetServiceRoleForAccountResponse' {associatedAt} -> associatedAt) (\s@GetServiceRoleForAccountResponse' {} a -> s {associatedAt = a} :: GetServiceRoleForAccountResponse)

-- | The response's http status code.
getServiceRoleForAccountResponse_httpStatus :: Lens.Lens' GetServiceRoleForAccountResponse Prelude.Int
getServiceRoleForAccountResponse_httpStatus = Lens.lens (\GetServiceRoleForAccountResponse' {httpStatus} -> httpStatus) (\s@GetServiceRoleForAccountResponse' {} a -> s {httpStatus = a} :: GetServiceRoleForAccountResponse)

instance
  Prelude.NFData
    GetServiceRoleForAccountResponse

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
-- Module      : Network.AWS.STS.GetCallerIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the IAM user or role whose credentials are used to
-- call the operation.
--
-- No permissions are required to perform this operation. If an
-- administrator adds a policy to your IAM user or role that explicitly
-- denies access to the @sts:GetCallerIdentity@ action, you can still
-- perform this operation. Permissions are not required because the same
-- information is returned when an IAM user or role is denied access. To
-- view an example response, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_access-denied-delete-mfa I Am Not Authorized to Perform: iam:DeleteVirtualMFADevice>
-- in the /IAM User Guide/.
module Network.AWS.STS.GetCallerIdentity
  ( -- * Creating a Request
    GetCallerIdentity (..),
    newGetCallerIdentity,

    -- * Destructuring the Response
    GetCallerIdentityResponse (..),
    newGetCallerIdentityResponse,

    -- * Response Lenses
    getCallerIdentityResponse_arn,
    getCallerIdentityResponse_userId,
    getCallerIdentityResponse_account,
    getCallerIdentityResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.STS.Types

-- | /See:/ 'newGetCallerIdentity' smart constructor.
data GetCallerIdentity = GetCallerIdentity'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCallerIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetCallerIdentity ::
  GetCallerIdentity
newGetCallerIdentity = GetCallerIdentity'

instance Prelude.AWSRequest GetCallerIdentity where
  type Rs GetCallerIdentity = GetCallerIdentityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetCallerIdentityResult"
      ( \s h x ->
          GetCallerIdentityResponse'
            Prelude.<$> (x Prelude..@? "Arn")
            Prelude.<*> (x Prelude..@? "UserId")
            Prelude.<*> (x Prelude..@? "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCallerIdentity

instance Prelude.NFData GetCallerIdentity

instance Prelude.ToHeaders GetCallerIdentity where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetCallerIdentity where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCallerIdentity where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("GetCallerIdentity" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2011-06-15" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful GetCallerIdentity request,
-- including information about the entity making the request.
--
-- /See:/ 'newGetCallerIdentityResponse' smart constructor.
data GetCallerIdentityResponse = GetCallerIdentityResponse'
  { -- | The AWS ARN associated with the calling entity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the calling entity. The exact value depends on
    -- the type of entity that is making the call. The values returned are
    -- those listed in the __aws:userid__ column in the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table>
    -- found on the __Policy Variables__ reference page in the /IAM User
    -- Guide/.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID number of the account that owns or contains the
    -- calling entity.
    account :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCallerIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getCallerIdentityResponse_arn' - The AWS ARN associated with the calling entity.
--
-- 'userId', 'getCallerIdentityResponse_userId' - The unique identifier of the calling entity. The exact value depends on
-- the type of entity that is making the call. The values returned are
-- those listed in the __aws:userid__ column in the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table>
-- found on the __Policy Variables__ reference page in the /IAM User
-- Guide/.
--
-- 'account', 'getCallerIdentityResponse_account' - The AWS account ID number of the account that owns or contains the
-- calling entity.
--
-- 'httpStatus', 'getCallerIdentityResponse_httpStatus' - The response's http status code.
newGetCallerIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCallerIdentityResponse
newGetCallerIdentityResponse pHttpStatus_ =
  GetCallerIdentityResponse'
    { arn = Prelude.Nothing,
      userId = Prelude.Nothing,
      account = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AWS ARN associated with the calling entity.
getCallerIdentityResponse_arn :: Lens.Lens' GetCallerIdentityResponse (Prelude.Maybe Prelude.Text)
getCallerIdentityResponse_arn = Lens.lens (\GetCallerIdentityResponse' {arn} -> arn) (\s@GetCallerIdentityResponse' {} a -> s {arn = a} :: GetCallerIdentityResponse)

-- | The unique identifier of the calling entity. The exact value depends on
-- the type of entity that is making the call. The values returned are
-- those listed in the __aws:userid__ column in the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table>
-- found on the __Policy Variables__ reference page in the /IAM User
-- Guide/.
getCallerIdentityResponse_userId :: Lens.Lens' GetCallerIdentityResponse (Prelude.Maybe Prelude.Text)
getCallerIdentityResponse_userId = Lens.lens (\GetCallerIdentityResponse' {userId} -> userId) (\s@GetCallerIdentityResponse' {} a -> s {userId = a} :: GetCallerIdentityResponse)

-- | The AWS account ID number of the account that owns or contains the
-- calling entity.
getCallerIdentityResponse_account :: Lens.Lens' GetCallerIdentityResponse (Prelude.Maybe Prelude.Text)
getCallerIdentityResponse_account = Lens.lens (\GetCallerIdentityResponse' {account} -> account) (\s@GetCallerIdentityResponse' {} a -> s {account = a} :: GetCallerIdentityResponse)

-- | The response's http status code.
getCallerIdentityResponse_httpStatus :: Lens.Lens' GetCallerIdentityResponse Prelude.Int
getCallerIdentityResponse_httpStatus = Lens.lens (\GetCallerIdentityResponse' {httpStatus} -> httpStatus) (\s@GetCallerIdentityResponse' {} a -> s {httpStatus = a} :: GetCallerIdentityResponse)

instance Prelude.NFData GetCallerIdentityResponse

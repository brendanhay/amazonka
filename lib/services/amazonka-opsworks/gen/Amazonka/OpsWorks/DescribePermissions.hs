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
-- Module      : Amazonka.OpsWorks.DescribePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a specified stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribePermissions
  ( -- * Creating a Request
    DescribePermissions (..),
    newDescribePermissions,

    -- * Request Lenses
    describePermissions_iamUserArn,
    describePermissions_stackId,

    -- * Destructuring the Response
    DescribePermissionsResponse (..),
    newDescribePermissionsResponse,

    -- * Response Lenses
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePermissions' smart constructor.
data DescribePermissions = DescribePermissions'
  { -- | The user\'s IAM ARN. This can also be a federated user\'s ARN. For more
    -- information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArn', 'describePermissions_iamUserArn' - The user\'s IAM ARN. This can also be a federated user\'s ARN. For more
-- information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'stackId', 'describePermissions_stackId' - The stack ID.
newDescribePermissions ::
  DescribePermissions
newDescribePermissions =
  DescribePermissions'
    { iamUserArn = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | The user\'s IAM ARN. This can also be a federated user\'s ARN. For more
-- information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
describePermissions_iamUserArn :: Lens.Lens' DescribePermissions (Prelude.Maybe Prelude.Text)
describePermissions_iamUserArn = Lens.lens (\DescribePermissions' {iamUserArn} -> iamUserArn) (\s@DescribePermissions' {} a -> s {iamUserArn = a} :: DescribePermissions)

-- | The stack ID.
describePermissions_stackId :: Lens.Lens' DescribePermissions (Prelude.Maybe Prelude.Text)
describePermissions_stackId = Lens.lens (\DescribePermissions' {stackId} -> stackId) (\s@DescribePermissions' {} a -> s {stackId = a} :: DescribePermissions)

instance Core.AWSRequest DescribePermissions where
  type
    AWSResponse DescribePermissions =
      DescribePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePermissionsResponse'
            Prelude.<$> (x Data..?> "Permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePermissions where
  hashWithSalt _salt DescribePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribePermissions where
  rnf DescribePermissions' {..} =
    Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribePermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePermissions where
  toJSON DescribePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IamUserArn" Data..=) Prelude.<$> iamUserArn,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribePermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribePermissions@ request.
--
-- /See:/ 'newDescribePermissionsResponse' smart constructor.
data DescribePermissionsResponse = DescribePermissionsResponse'
  { -- | An array of @Permission@ objects that describe the stack permissions.
    --
    -- -   If the request object contains only a stack ID, the array contains a
    --     @Permission@ object with permissions for each of the stack IAM ARNs.
    --
    -- -   If the request object contains only an IAM ARN, the array contains a
    --     @Permission@ object with permissions for each of the user\'s stack
    --     IDs.
    --
    -- -   If the request contains a stack ID and an IAM ARN, the array
    --     contains a single @Permission@ object with permissions for the
    --     specified stack and IAM ARN.
    permissions :: Prelude.Maybe [Permission],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'describePermissionsResponse_permissions' - An array of @Permission@ objects that describe the stack permissions.
--
-- -   If the request object contains only a stack ID, the array contains a
--     @Permission@ object with permissions for each of the stack IAM ARNs.
--
-- -   If the request object contains only an IAM ARN, the array contains a
--     @Permission@ object with permissions for each of the user\'s stack
--     IDs.
--
-- -   If the request contains a stack ID and an IAM ARN, the array
--     contains a single @Permission@ object with permissions for the
--     specified stack and IAM ARN.
--
-- 'httpStatus', 'describePermissionsResponse_httpStatus' - The response's http status code.
newDescribePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePermissionsResponse
newDescribePermissionsResponse pHttpStatus_ =
  DescribePermissionsResponse'
    { permissions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Permission@ objects that describe the stack permissions.
--
-- -   If the request object contains only a stack ID, the array contains a
--     @Permission@ object with permissions for each of the stack IAM ARNs.
--
-- -   If the request object contains only an IAM ARN, the array contains a
--     @Permission@ object with permissions for each of the user\'s stack
--     IDs.
--
-- -   If the request contains a stack ID and an IAM ARN, the array
--     contains a single @Permission@ object with permissions for the
--     specified stack and IAM ARN.
describePermissionsResponse_permissions :: Lens.Lens' DescribePermissionsResponse (Prelude.Maybe [Permission])
describePermissionsResponse_permissions = Lens.lens (\DescribePermissionsResponse' {permissions} -> permissions) (\s@DescribePermissionsResponse' {} a -> s {permissions = a} :: DescribePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePermissionsResponse_httpStatus :: Lens.Lens' DescribePermissionsResponse Prelude.Int
describePermissionsResponse_httpStatus = Lens.lens (\DescribePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribePermissionsResponse' {} a -> s {httpStatus = a} :: DescribePermissionsResponse)

instance Prelude.NFData DescribePermissionsResponse where
  rnf DescribePermissionsResponse' {..} =
    Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus

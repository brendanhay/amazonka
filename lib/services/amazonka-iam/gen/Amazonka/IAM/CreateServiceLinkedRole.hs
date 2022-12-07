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
-- Module      : Amazonka.IAM.CreateServiceLinkedRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM role that is linked to a specific Amazon Web Services
-- service. The service controls the attached policies and when the role
-- can be deleted. This helps ensure that the service is not broken by an
-- unexpectedly changed or deleted role, which could put your Amazon Web
-- Services resources into an unknown state. Allowing the service to
-- control the role helps improve service stability and proper cleanup when
-- a service and its role are no longer needed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html Using service-linked roles>
-- in the /IAM User Guide/.
--
-- To attach a policy to this service-linked role, you must make the
-- request using the Amazon Web Services service that depends on this role.
module Amazonka.IAM.CreateServiceLinkedRole
  ( -- * Creating a Request
    CreateServiceLinkedRole (..),
    newCreateServiceLinkedRole,

    -- * Request Lenses
    createServiceLinkedRole_customSuffix,
    createServiceLinkedRole_description,
    createServiceLinkedRole_aWSServiceName,

    -- * Destructuring the Response
    CreateServiceLinkedRoleResponse (..),
    newCreateServiceLinkedRoleResponse,

    -- * Response Lenses
    createServiceLinkedRoleResponse_role,
    createServiceLinkedRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceLinkedRole' smart constructor.
data CreateServiceLinkedRole = CreateServiceLinkedRole'
  { -- | A string that you provide, which is combined with the service-provided
    -- prefix to form the complete role name. If you make multiple requests for
    -- the same service, then you must supply a different @CustomSuffix@ for
    -- each request. Otherwise the request fails with a duplicate role name
    -- error. For example, you could add @-1@ or @-debug@ to the suffix.
    --
    -- Some services do not support the @CustomSuffix@ parameter. If you
    -- provide an optional suffix and the operation fails, try the operation
    -- again without the suffix.
    customSuffix :: Prelude.Maybe Prelude.Text,
    -- | The description of the role.
    description :: Prelude.Maybe Prelude.Text,
    -- | The service principal for the Amazon Web Services service to which this
    -- role is attached. You use a string similar to a URL but without the
    -- http:\/\/ in front. For example: @elasticbeanstalk.amazonaws.com@.
    --
    -- Service principals are unique and case-sensitive. To find the exact
    -- service principal for your service-linked role, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html Amazon Web Services services that work with IAM>
    -- in the /IAM User Guide/. Look for the services that have __Yes__ in the
    -- __Service-Linked Role__ column. Choose the __Yes__ link to view the
    -- service-linked role documentation for that service.
    aWSServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceLinkedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customSuffix', 'createServiceLinkedRole_customSuffix' - A string that you provide, which is combined with the service-provided
-- prefix to form the complete role name. If you make multiple requests for
-- the same service, then you must supply a different @CustomSuffix@ for
-- each request. Otherwise the request fails with a duplicate role name
-- error. For example, you could add @-1@ or @-debug@ to the suffix.
--
-- Some services do not support the @CustomSuffix@ parameter. If you
-- provide an optional suffix and the operation fails, try the operation
-- again without the suffix.
--
-- 'description', 'createServiceLinkedRole_description' - The description of the role.
--
-- 'aWSServiceName', 'createServiceLinkedRole_aWSServiceName' - The service principal for the Amazon Web Services service to which this
-- role is attached. You use a string similar to a URL but without the
-- http:\/\/ in front. For example: @elasticbeanstalk.amazonaws.com@.
--
-- Service principals are unique and case-sensitive. To find the exact
-- service principal for your service-linked role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html Amazon Web Services services that work with IAM>
-- in the /IAM User Guide/. Look for the services that have __Yes__ in the
-- __Service-Linked Role__ column. Choose the __Yes__ link to view the
-- service-linked role documentation for that service.
newCreateServiceLinkedRole ::
  -- | 'aWSServiceName'
  Prelude.Text ->
  CreateServiceLinkedRole
newCreateServiceLinkedRole pAWSServiceName_ =
  CreateServiceLinkedRole'
    { customSuffix =
        Prelude.Nothing,
      description = Prelude.Nothing,
      aWSServiceName = pAWSServiceName_
    }

-- | A string that you provide, which is combined with the service-provided
-- prefix to form the complete role name. If you make multiple requests for
-- the same service, then you must supply a different @CustomSuffix@ for
-- each request. Otherwise the request fails with a duplicate role name
-- error. For example, you could add @-1@ or @-debug@ to the suffix.
--
-- Some services do not support the @CustomSuffix@ parameter. If you
-- provide an optional suffix and the operation fails, try the operation
-- again without the suffix.
createServiceLinkedRole_customSuffix :: Lens.Lens' CreateServiceLinkedRole (Prelude.Maybe Prelude.Text)
createServiceLinkedRole_customSuffix = Lens.lens (\CreateServiceLinkedRole' {customSuffix} -> customSuffix) (\s@CreateServiceLinkedRole' {} a -> s {customSuffix = a} :: CreateServiceLinkedRole)

-- | The description of the role.
createServiceLinkedRole_description :: Lens.Lens' CreateServiceLinkedRole (Prelude.Maybe Prelude.Text)
createServiceLinkedRole_description = Lens.lens (\CreateServiceLinkedRole' {description} -> description) (\s@CreateServiceLinkedRole' {} a -> s {description = a} :: CreateServiceLinkedRole)

-- | The service principal for the Amazon Web Services service to which this
-- role is attached. You use a string similar to a URL but without the
-- http:\/\/ in front. For example: @elasticbeanstalk.amazonaws.com@.
--
-- Service principals are unique and case-sensitive. To find the exact
-- service principal for your service-linked role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html Amazon Web Services services that work with IAM>
-- in the /IAM User Guide/. Look for the services that have __Yes__ in the
-- __Service-Linked Role__ column. Choose the __Yes__ link to view the
-- service-linked role documentation for that service.
createServiceLinkedRole_aWSServiceName :: Lens.Lens' CreateServiceLinkedRole Prelude.Text
createServiceLinkedRole_aWSServiceName = Lens.lens (\CreateServiceLinkedRole' {aWSServiceName} -> aWSServiceName) (\s@CreateServiceLinkedRole' {} a -> s {aWSServiceName = a} :: CreateServiceLinkedRole)

instance Core.AWSRequest CreateServiceLinkedRole where
  type
    AWSResponse CreateServiceLinkedRole =
      CreateServiceLinkedRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateServiceLinkedRoleResult"
      ( \s h x ->
          CreateServiceLinkedRoleResponse'
            Prelude.<$> (x Data..@? "Role")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceLinkedRole where
  hashWithSalt _salt CreateServiceLinkedRole' {..} =
    _salt `Prelude.hashWithSalt` customSuffix
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` aWSServiceName

instance Prelude.NFData CreateServiceLinkedRole where
  rnf CreateServiceLinkedRole' {..} =
    Prelude.rnf customSuffix
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf aWSServiceName

instance Data.ToHeaders CreateServiceLinkedRole where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateServiceLinkedRole where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceLinkedRole where
  toQuery CreateServiceLinkedRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateServiceLinkedRole" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "CustomSuffix" Data.=: customSuffix,
        "Description" Data.=: description,
        "AWSServiceName" Data.=: aWSServiceName
      ]

-- | /See:/ 'newCreateServiceLinkedRoleResponse' smart constructor.
data CreateServiceLinkedRoleResponse = CreateServiceLinkedRoleResponse'
  { -- | A Role object that contains details about the newly created role.
    role' :: Prelude.Maybe Role,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceLinkedRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'createServiceLinkedRoleResponse_role' - A Role object that contains details about the newly created role.
--
-- 'httpStatus', 'createServiceLinkedRoleResponse_httpStatus' - The response's http status code.
newCreateServiceLinkedRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceLinkedRoleResponse
newCreateServiceLinkedRoleResponse pHttpStatus_ =
  CreateServiceLinkedRoleResponse'
    { role' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Role object that contains details about the newly created role.
createServiceLinkedRoleResponse_role :: Lens.Lens' CreateServiceLinkedRoleResponse (Prelude.Maybe Role)
createServiceLinkedRoleResponse_role = Lens.lens (\CreateServiceLinkedRoleResponse' {role'} -> role') (\s@CreateServiceLinkedRoleResponse' {} a -> s {role' = a} :: CreateServiceLinkedRoleResponse)

-- | The response's http status code.
createServiceLinkedRoleResponse_httpStatus :: Lens.Lens' CreateServiceLinkedRoleResponse Prelude.Int
createServiceLinkedRoleResponse_httpStatus = Lens.lens (\CreateServiceLinkedRoleResponse' {httpStatus} -> httpStatus) (\s@CreateServiceLinkedRoleResponse' {} a -> s {httpStatus = a} :: CreateServiceLinkedRoleResponse)

instance
  Prelude.NFData
    CreateServiceLinkedRoleResponse
  where
  rnf CreateServiceLinkedRoleResponse' {..} =
    Prelude.rnf role'
      `Prelude.seq` Prelude.rnf httpStatus

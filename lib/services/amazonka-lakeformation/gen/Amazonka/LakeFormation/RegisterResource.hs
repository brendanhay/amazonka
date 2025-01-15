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
-- Module      : Amazonka.LakeFormation.RegisterResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the resource as managed by the Data Catalog.
--
-- To add or update data, Lake Formation needs read\/write access to the
-- chosen Amazon S3 path. Choose a role that you know has permission to do
-- this, or choose the AWSServiceRoleForLakeFormationDataAccess
-- service-linked role. When you register the first Amazon S3 path, the
-- service-linked role and a new inline policy are created on your behalf.
-- Lake Formation adds the first path to the inline policy and attaches it
-- to the service-linked role. When you register subsequent paths, Lake
-- Formation adds the path to the existing policy.
--
-- The following request registers a new location and gives Lake Formation
-- permission to use the service-linked role to access that location.
--
-- @ResourceArn = arn:aws:s3:::my-bucket UseServiceLinkedRole = true@
--
-- If @UseServiceLinkedRole@ is not set to true, you must provide or set
-- the @RoleArn@:
--
-- @arn:aws:iam::12345:role\/my-data-access-role@
module Amazonka.LakeFormation.RegisterResource
  ( -- * Creating a Request
    RegisterResource (..),
    newRegisterResource,

    -- * Request Lenses
    registerResource_roleArn,
    registerResource_useServiceLinkedRole,
    registerResource_resourceArn,

    -- * Destructuring the Response
    RegisterResourceResponse (..),
    newRegisterResourceResponse,

    -- * Response Lenses
    registerResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterResource' smart constructor.
data RegisterResource = RegisterResource'
  { -- | The identifier for the role that registers the resource.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Designates an Identity and Access Management (IAM) service-linked role
    -- by registering this role with the Data Catalog. A service-linked role is
    -- a unique type of IAM role that is linked directly to Lake Formation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lake-formation/latest/dg/service-linked-roles.html Using Service-Linked Roles for Lake Formation>.
    useServiceLinkedRole :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the resource that you want to
    -- register.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'registerResource_roleArn' - The identifier for the role that registers the resource.
--
-- 'useServiceLinkedRole', 'registerResource_useServiceLinkedRole' - Designates an Identity and Access Management (IAM) service-linked role
-- by registering this role with the Data Catalog. A service-linked role is
-- a unique type of IAM role that is linked directly to Lake Formation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/service-linked-roles.html Using Service-Linked Roles for Lake Formation>.
--
-- 'resourceArn', 'registerResource_resourceArn' - The Amazon Resource Name (ARN) of the resource that you want to
-- register.
newRegisterResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  RegisterResource
newRegisterResource pResourceArn_ =
  RegisterResource'
    { roleArn = Prelude.Nothing,
      useServiceLinkedRole = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The identifier for the role that registers the resource.
registerResource_roleArn :: Lens.Lens' RegisterResource (Prelude.Maybe Prelude.Text)
registerResource_roleArn = Lens.lens (\RegisterResource' {roleArn} -> roleArn) (\s@RegisterResource' {} a -> s {roleArn = a} :: RegisterResource)

-- | Designates an Identity and Access Management (IAM) service-linked role
-- by registering this role with the Data Catalog. A service-linked role is
-- a unique type of IAM role that is linked directly to Lake Formation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/service-linked-roles.html Using Service-Linked Roles for Lake Formation>.
registerResource_useServiceLinkedRole :: Lens.Lens' RegisterResource (Prelude.Maybe Prelude.Bool)
registerResource_useServiceLinkedRole = Lens.lens (\RegisterResource' {useServiceLinkedRole} -> useServiceLinkedRole) (\s@RegisterResource' {} a -> s {useServiceLinkedRole = a} :: RegisterResource)

-- | The Amazon Resource Name (ARN) of the resource that you want to
-- register.
registerResource_resourceArn :: Lens.Lens' RegisterResource Prelude.Text
registerResource_resourceArn = Lens.lens (\RegisterResource' {resourceArn} -> resourceArn) (\s@RegisterResource' {} a -> s {resourceArn = a} :: RegisterResource)

instance Core.AWSRequest RegisterResource where
  type
    AWSResponse RegisterResource =
      RegisterResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterResource where
  hashWithSalt _salt RegisterResource' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` useServiceLinkedRole
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData RegisterResource where
  rnf RegisterResource' {..} =
    Prelude.rnf roleArn `Prelude.seq`
      Prelude.rnf useServiceLinkedRole `Prelude.seq`
        Prelude.rnf resourceArn

instance Data.ToHeaders RegisterResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterResource where
  toJSON RegisterResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("UseServiceLinkedRole" Data..=)
              Prelude.<$> useServiceLinkedRole,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath RegisterResource where
  toPath = Prelude.const "/RegisterResource"

instance Data.ToQuery RegisterResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterResourceResponse' smart constructor.
data RegisterResourceResponse = RegisterResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerResourceResponse_httpStatus' - The response's http status code.
newRegisterResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterResourceResponse
newRegisterResourceResponse pHttpStatus_ =
  RegisterResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerResourceResponse_httpStatus :: Lens.Lens' RegisterResourceResponse Prelude.Int
registerResourceResponse_httpStatus = Lens.lens (\RegisterResourceResponse' {httpStatus} -> httpStatus) (\s@RegisterResourceResponse' {} a -> s {httpStatus = a} :: RegisterResourceResponse)

instance Prelude.NFData RegisterResourceResponse where
  rnf RegisterResourceResponse' {..} =
    Prelude.rnf httpStatus

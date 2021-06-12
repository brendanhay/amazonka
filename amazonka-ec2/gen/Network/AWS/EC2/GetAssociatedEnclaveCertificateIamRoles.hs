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
-- Module      : Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM roles that are associated with the specified AWS
-- Certificate Manager (ACM) certificate. It also returns the name of the
-- Amazon S3 bucket and the Amazon S3 object key where the certificate,
-- certificate chain, and encrypted private key bundle are stored, and the
-- ARN of the AWS Key Management Service (KMS) customer master key (CMK)
-- that\'s used to encrypt the private key.
module Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
  ( -- * Creating a Request
    GetAssociatedEnclaveCertificateIamRoles (..),
    newGetAssociatedEnclaveCertificateIamRoles,

    -- * Request Lenses
    getAssociatedEnclaveCertificateIamRoles_dryRun,
    getAssociatedEnclaveCertificateIamRoles_certificateArn,

    -- * Destructuring the Response
    GetAssociatedEnclaveCertificateIamRolesResponse (..),
    newGetAssociatedEnclaveCertificateIamRolesResponse,

    -- * Response Lenses
    getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles,
    getAssociatedEnclaveCertificateIamRolesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssociatedEnclaveCertificateIamRoles' smart constructor.
data GetAssociatedEnclaveCertificateIamRoles = GetAssociatedEnclaveCertificateIamRoles'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ARN of the ACM certificate for which to view the associated IAM
    -- roles, encryption keys, and Amazon S3 object information.
    certificateArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedEnclaveCertificateIamRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getAssociatedEnclaveCertificateIamRoles_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'certificateArn', 'getAssociatedEnclaveCertificateIamRoles_certificateArn' - The ARN of the ACM certificate for which to view the associated IAM
-- roles, encryption keys, and Amazon S3 object information.
newGetAssociatedEnclaveCertificateIamRoles ::
  GetAssociatedEnclaveCertificateIamRoles
newGetAssociatedEnclaveCertificateIamRoles =
  GetAssociatedEnclaveCertificateIamRoles'
    { dryRun =
        Core.Nothing,
      certificateArn = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getAssociatedEnclaveCertificateIamRoles_dryRun :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Core.Maybe Core.Bool)
getAssociatedEnclaveCertificateIamRoles_dryRun = Lens.lens (\GetAssociatedEnclaveCertificateIamRoles' {dryRun} -> dryRun) (\s@GetAssociatedEnclaveCertificateIamRoles' {} a -> s {dryRun = a} :: GetAssociatedEnclaveCertificateIamRoles)

-- | The ARN of the ACM certificate for which to view the associated IAM
-- roles, encryption keys, and Amazon S3 object information.
getAssociatedEnclaveCertificateIamRoles_certificateArn :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Core.Maybe Core.Text)
getAssociatedEnclaveCertificateIamRoles_certificateArn = Lens.lens (\GetAssociatedEnclaveCertificateIamRoles' {certificateArn} -> certificateArn) (\s@GetAssociatedEnclaveCertificateIamRoles' {} a -> s {certificateArn = a} :: GetAssociatedEnclaveCertificateIamRoles)

instance
  Core.AWSRequest
    GetAssociatedEnclaveCertificateIamRoles
  where
  type
    AWSResponse
      GetAssociatedEnclaveCertificateIamRoles =
      GetAssociatedEnclaveCertificateIamRolesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetAssociatedEnclaveCertificateIamRolesResponse'
            Core.<$> ( x Core..@? "associatedRoleSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetAssociatedEnclaveCertificateIamRoles

instance
  Core.NFData
    GetAssociatedEnclaveCertificateIamRoles

instance
  Core.ToHeaders
    GetAssociatedEnclaveCertificateIamRoles
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetAssociatedEnclaveCertificateIamRoles
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetAssociatedEnclaveCertificateIamRoles
  where
  toQuery GetAssociatedEnclaveCertificateIamRoles' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetAssociatedEnclaveCertificateIamRoles" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "CertificateArn" Core.=: certificateArn
      ]

-- | /See:/ 'newGetAssociatedEnclaveCertificateIamRolesResponse' smart constructor.
data GetAssociatedEnclaveCertificateIamRolesResponse = GetAssociatedEnclaveCertificateIamRolesResponse'
  { -- | Information about the associated IAM roles.
    associatedRoles :: Core.Maybe [AssociatedRole],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedEnclaveCertificateIamRolesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedRoles', 'getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles' - Information about the associated IAM roles.
--
-- 'httpStatus', 'getAssociatedEnclaveCertificateIamRolesResponse_httpStatus' - The response's http status code.
newGetAssociatedEnclaveCertificateIamRolesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAssociatedEnclaveCertificateIamRolesResponse
newGetAssociatedEnclaveCertificateIamRolesResponse
  pHttpStatus_ =
    GetAssociatedEnclaveCertificateIamRolesResponse'
      { associatedRoles =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the associated IAM roles.
getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse (Core.Maybe [AssociatedRole])
getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles = Lens.lens (\GetAssociatedEnclaveCertificateIamRolesResponse' {associatedRoles} -> associatedRoles) (\s@GetAssociatedEnclaveCertificateIamRolesResponse' {} a -> s {associatedRoles = a} :: GetAssociatedEnclaveCertificateIamRolesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAssociatedEnclaveCertificateIamRolesResponse_httpStatus :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse Core.Int
getAssociatedEnclaveCertificateIamRolesResponse_httpStatus = Lens.lens (\GetAssociatedEnclaveCertificateIamRolesResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedEnclaveCertificateIamRolesResponse' {} a -> s {httpStatus = a} :: GetAssociatedEnclaveCertificateIamRolesResponse)

instance
  Core.NFData
    GetAssociatedEnclaveCertificateIamRolesResponse

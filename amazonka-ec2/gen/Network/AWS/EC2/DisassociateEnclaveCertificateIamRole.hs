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
-- Module      : Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM role from an AWS Certificate Manager (ACM)
-- certificate. Disassociating an IAM role from an ACM certificate removes
-- the Amazon S3 object that contains the certificate, certificate chain,
-- and encrypted private key from the Amazon S3 bucket. It also revokes the
-- IAM role\'s permission to use the AWS Key Management Service (KMS)
-- customer master key (CMK) used to encrypt the private key. This
-- effectively revokes the role\'s permission to use the certificate.
module Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
  ( -- * Creating a Request
    DisassociateEnclaveCertificateIamRole (..),
    newDisassociateEnclaveCertificateIamRole,

    -- * Request Lenses
    disassociateEnclaveCertificateIamRole_roleArn,
    disassociateEnclaveCertificateIamRole_dryRun,
    disassociateEnclaveCertificateIamRole_certificateArn,

    -- * Destructuring the Response
    DisassociateEnclaveCertificateIamRoleResponse (..),
    newDisassociateEnclaveCertificateIamRoleResponse,

    -- * Response Lenses
    disassociateEnclaveCertificateIamRoleResponse_return,
    disassociateEnclaveCertificateIamRoleResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateEnclaveCertificateIamRole' smart constructor.
data DisassociateEnclaveCertificateIamRole = DisassociateEnclaveCertificateIamRole'
  { -- | The ARN of the IAM role to disassociate.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the ACM certificate from which to disassociate the IAM role.
    certificateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateEnclaveCertificateIamRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'disassociateEnclaveCertificateIamRole_roleArn' - The ARN of the IAM role to disassociate.
--
-- 'dryRun', 'disassociateEnclaveCertificateIamRole_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'certificateArn', 'disassociateEnclaveCertificateIamRole_certificateArn' - The ARN of the ACM certificate from which to disassociate the IAM role.
newDisassociateEnclaveCertificateIamRole ::
  DisassociateEnclaveCertificateIamRole
newDisassociateEnclaveCertificateIamRole =
  DisassociateEnclaveCertificateIamRole'
    { roleArn =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      certificateArn = Prelude.Nothing
    }

-- | The ARN of the IAM role to disassociate.
disassociateEnclaveCertificateIamRole_roleArn :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Text)
disassociateEnclaveCertificateIamRole_roleArn = Lens.lens (\DisassociateEnclaveCertificateIamRole' {roleArn} -> roleArn) (\s@DisassociateEnclaveCertificateIamRole' {} a -> s {roleArn = a} :: DisassociateEnclaveCertificateIamRole)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disassociateEnclaveCertificateIamRole_dryRun :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Bool)
disassociateEnclaveCertificateIamRole_dryRun = Lens.lens (\DisassociateEnclaveCertificateIamRole' {dryRun} -> dryRun) (\s@DisassociateEnclaveCertificateIamRole' {} a -> s {dryRun = a} :: DisassociateEnclaveCertificateIamRole)

-- | The ARN of the ACM certificate from which to disassociate the IAM role.
disassociateEnclaveCertificateIamRole_certificateArn :: Lens.Lens' DisassociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Text)
disassociateEnclaveCertificateIamRole_certificateArn = Lens.lens (\DisassociateEnclaveCertificateIamRole' {certificateArn} -> certificateArn) (\s@DisassociateEnclaveCertificateIamRole' {} a -> s {certificateArn = a} :: DisassociateEnclaveCertificateIamRole)

instance
  Prelude.AWSRequest
    DisassociateEnclaveCertificateIamRole
  where
  type
    Rs DisassociateEnclaveCertificateIamRole =
      DisassociateEnclaveCertificateIamRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateEnclaveCertificateIamRoleResponse'
            Prelude.<$> (x Prelude..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateEnclaveCertificateIamRole

instance
  Prelude.NFData
    DisassociateEnclaveCertificateIamRole

instance
  Prelude.ToHeaders
    DisassociateEnclaveCertificateIamRole
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DisassociateEnclaveCertificateIamRole
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateEnclaveCertificateIamRole
  where
  toQuery DisassociateEnclaveCertificateIamRole' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DisassociateEnclaveCertificateIamRole" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "RoleArn" Prelude.=: roleArn,
        "DryRun" Prelude.=: dryRun,
        "CertificateArn" Prelude.=: certificateArn
      ]

-- | /See:/ 'newDisassociateEnclaveCertificateIamRoleResponse' smart constructor.
data DisassociateEnclaveCertificateIamRoleResponse = DisassociateEnclaveCertificateIamRoleResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateEnclaveCertificateIamRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'disassociateEnclaveCertificateIamRoleResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disassociateEnclaveCertificateIamRoleResponse_httpStatus' - The response's http status code.
newDisassociateEnclaveCertificateIamRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateEnclaveCertificateIamRoleResponse
newDisassociateEnclaveCertificateIamRoleResponse
  pHttpStatus_ =
    DisassociateEnclaveCertificateIamRoleResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disassociateEnclaveCertificateIamRoleResponse_return :: Lens.Lens' DisassociateEnclaveCertificateIamRoleResponse (Prelude.Maybe Prelude.Bool)
disassociateEnclaveCertificateIamRoleResponse_return = Lens.lens (\DisassociateEnclaveCertificateIamRoleResponse' {return'} -> return') (\s@DisassociateEnclaveCertificateIamRoleResponse' {} a -> s {return' = a} :: DisassociateEnclaveCertificateIamRoleResponse)

-- | The response's http status code.
disassociateEnclaveCertificateIamRoleResponse_httpStatus :: Lens.Lens' DisassociateEnclaveCertificateIamRoleResponse Prelude.Int
disassociateEnclaveCertificateIamRoleResponse_httpStatus = Lens.lens (\DisassociateEnclaveCertificateIamRoleResponse' {httpStatus} -> httpStatus) (\s@DisassociateEnclaveCertificateIamRoleResponse' {} a -> s {httpStatus = a} :: DisassociateEnclaveCertificateIamRoleResponse)

instance
  Prelude.NFData
    DisassociateEnclaveCertificateIamRoleResponse

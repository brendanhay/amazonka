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
-- Module      : Amazonka.EC2.GetAssociatedEnclaveCertificateIamRoles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM roles that are associated with the specified ACM (ACM)
-- certificate. It also returns the name of the Amazon S3 bucket and the
-- Amazon S3 object key where the certificate, certificate chain, and
-- encrypted private key bundle are stored, and the ARN of the KMS key
-- that\'s used to encrypt the private key.
module Amazonka.EC2.GetAssociatedEnclaveCertificateIamRoles
  ( -- * Creating a Request
    GetAssociatedEnclaveCertificateIamRoles (..),
    newGetAssociatedEnclaveCertificateIamRoles,

    -- * Request Lenses
    getAssociatedEnclaveCertificateIamRoles_certificateArn,
    getAssociatedEnclaveCertificateIamRoles_dryRun,

    -- * Destructuring the Response
    GetAssociatedEnclaveCertificateIamRolesResponse (..),
    newGetAssociatedEnclaveCertificateIamRolesResponse,

    -- * Response Lenses
    getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles,
    getAssociatedEnclaveCertificateIamRolesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssociatedEnclaveCertificateIamRoles' smart constructor.
data GetAssociatedEnclaveCertificateIamRoles = GetAssociatedEnclaveCertificateIamRoles'
  { -- | The ARN of the ACM certificate for which to view the associated IAM
    -- roles, encryption keys, and Amazon S3 object information.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedEnclaveCertificateIamRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'getAssociatedEnclaveCertificateIamRoles_certificateArn' - The ARN of the ACM certificate for which to view the associated IAM
-- roles, encryption keys, and Amazon S3 object information.
--
-- 'dryRun', 'getAssociatedEnclaveCertificateIamRoles_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newGetAssociatedEnclaveCertificateIamRoles ::
  GetAssociatedEnclaveCertificateIamRoles
newGetAssociatedEnclaveCertificateIamRoles =
  GetAssociatedEnclaveCertificateIamRoles'
    { certificateArn =
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The ARN of the ACM certificate for which to view the associated IAM
-- roles, encryption keys, and Amazon S3 object information.
getAssociatedEnclaveCertificateIamRoles_certificateArn :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Prelude.Maybe Prelude.Text)
getAssociatedEnclaveCertificateIamRoles_certificateArn = Lens.lens (\GetAssociatedEnclaveCertificateIamRoles' {certificateArn} -> certificateArn) (\s@GetAssociatedEnclaveCertificateIamRoles' {} a -> s {certificateArn = a} :: GetAssociatedEnclaveCertificateIamRoles)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getAssociatedEnclaveCertificateIamRoles_dryRun :: Lens.Lens' GetAssociatedEnclaveCertificateIamRoles (Prelude.Maybe Prelude.Bool)
getAssociatedEnclaveCertificateIamRoles_dryRun = Lens.lens (\GetAssociatedEnclaveCertificateIamRoles' {dryRun} -> dryRun) (\s@GetAssociatedEnclaveCertificateIamRoles' {} a -> s {dryRun = a} :: GetAssociatedEnclaveCertificateIamRoles)

instance
  Core.AWSRequest
    GetAssociatedEnclaveCertificateIamRoles
  where
  type
    AWSResponse
      GetAssociatedEnclaveCertificateIamRoles =
      GetAssociatedEnclaveCertificateIamRolesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetAssociatedEnclaveCertificateIamRolesResponse'
            Prelude.<$> ( x
                            Data..@? "associatedRoleSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAssociatedEnclaveCertificateIamRoles
  where
  hashWithSalt
    _salt
    GetAssociatedEnclaveCertificateIamRoles' {..} =
      _salt
        `Prelude.hashWithSalt` certificateArn
        `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    GetAssociatedEnclaveCertificateIamRoles
  where
  rnf GetAssociatedEnclaveCertificateIamRoles' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf dryRun

instance
  Data.ToHeaders
    GetAssociatedEnclaveCertificateIamRoles
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetAssociatedEnclaveCertificateIamRoles
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetAssociatedEnclaveCertificateIamRoles
  where
  toQuery GetAssociatedEnclaveCertificateIamRoles' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetAssociatedEnclaveCertificateIamRoles" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CertificateArn" Data.=: certificateArn,
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newGetAssociatedEnclaveCertificateIamRolesResponse' smart constructor.
data GetAssociatedEnclaveCertificateIamRolesResponse = GetAssociatedEnclaveCertificateIamRolesResponse'
  { -- | Information about the associated IAM roles.
    associatedRoles :: Prelude.Maybe [AssociatedRole],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetAssociatedEnclaveCertificateIamRolesResponse
newGetAssociatedEnclaveCertificateIamRolesResponse
  pHttpStatus_ =
    GetAssociatedEnclaveCertificateIamRolesResponse'
      { associatedRoles =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the associated IAM roles.
getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse (Prelude.Maybe [AssociatedRole])
getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles = Lens.lens (\GetAssociatedEnclaveCertificateIamRolesResponse' {associatedRoles} -> associatedRoles) (\s@GetAssociatedEnclaveCertificateIamRolesResponse' {} a -> s {associatedRoles = a} :: GetAssociatedEnclaveCertificateIamRolesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAssociatedEnclaveCertificateIamRolesResponse_httpStatus :: Lens.Lens' GetAssociatedEnclaveCertificateIamRolesResponse Prelude.Int
getAssociatedEnclaveCertificateIamRolesResponse_httpStatus = Lens.lens (\GetAssociatedEnclaveCertificateIamRolesResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedEnclaveCertificateIamRolesResponse' {} a -> s {httpStatus = a} :: GetAssociatedEnclaveCertificateIamRolesResponse)

instance
  Prelude.NFData
    GetAssociatedEnclaveCertificateIamRolesResponse
  where
  rnf
    GetAssociatedEnclaveCertificateIamRolesResponse' {..} =
      Prelude.rnf associatedRoles
        `Prelude.seq` Prelude.rnf httpStatus

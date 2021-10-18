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
-- Module      : Network.AWS.CertificateManagerPCA.CreatePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants one or more permissions on a private CA to the AWS Certificate
-- Manager (ACM) service principal (@acm.amazonaws.com@). These permissions
-- allow ACM to issue and renew ACM certificates that reside in the same
-- AWS account as the CA.
--
-- You can list current permissions with the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions>
-- action and revoke them with the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission>
-- action.
--
-- __About Permissions__
--
-- -   If the private CA and the certificates it issues reside in the same
--     account, you can use @CreatePermission@ to grant permissions for ACM
--     to carry out automatic certificate renewals.
--
-- -   For automatic certificate renewal to succeed, the ACM service
--     principal needs permissions to create, retrieve, and list
--     certificates.
--
-- -   If the private CA and the ACM certificates reside in different
--     accounts, then permissions cannot be used to enable automatic
--     renewals. Instead, the ACM certificate owner must set up a
--     resource-based policy to enable cross-account issuance and renewals.
--     For more information, see
--     <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA>.
module Network.AWS.CertificateManagerPCA.CreatePermission
  ( -- * Creating a Request
    CreatePermission (..),
    newCreatePermission,

    -- * Request Lenses
    createPermission_sourceAccount,
    createPermission_certificateAuthorityArn,
    createPermission_principal,
    createPermission_actions,

    -- * Destructuring the Response
    CreatePermissionResponse (..),
    newCreatePermissionResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePermission' smart constructor.
data CreatePermission = CreatePermission'
  { -- | The ID of the calling account.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CA that grants the permissions.
    -- You can find the ARN by calling the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
    -- action. This must have the following form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Text,
    -- | The AWS service or identity that receives the permission. At this time,
    -- the only valid principal is @acm.amazonaws.com@.
    principal :: Prelude.Text,
    -- | The actions that the specified AWS service principal can use. These
    -- include @IssueCertificate@, @GetCertificate@, and @ListPermissions@.
    actions :: Prelude.NonEmpty ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAccount', 'createPermission_sourceAccount' - The ID of the calling account.
--
-- 'certificateAuthorityArn', 'createPermission_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the CA that grants the permissions.
-- You can find the ARN by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. This must have the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
--
-- 'principal', 'createPermission_principal' - The AWS service or identity that receives the permission. At this time,
-- the only valid principal is @acm.amazonaws.com@.
--
-- 'actions', 'createPermission_actions' - The actions that the specified AWS service principal can use. These
-- include @IssueCertificate@, @GetCertificate@, and @ListPermissions@.
newCreatePermission ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  -- | 'actions'
  Prelude.NonEmpty ActionType ->
  CreatePermission
newCreatePermission
  pCertificateAuthorityArn_
  pPrincipal_
  pActions_ =
    CreatePermission'
      { sourceAccount = Prelude.Nothing,
        certificateAuthorityArn = pCertificateAuthorityArn_,
        principal = pPrincipal_,
        actions = Lens._Coerce Lens.# pActions_
      }

-- | The ID of the calling account.
createPermission_sourceAccount :: Lens.Lens' CreatePermission (Prelude.Maybe Prelude.Text)
createPermission_sourceAccount = Lens.lens (\CreatePermission' {sourceAccount} -> sourceAccount) (\s@CreatePermission' {} a -> s {sourceAccount = a} :: CreatePermission)

-- | The Amazon Resource Name (ARN) of the CA that grants the permissions.
-- You can find the ARN by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. This must have the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
createPermission_certificateAuthorityArn :: Lens.Lens' CreatePermission Prelude.Text
createPermission_certificateAuthorityArn = Lens.lens (\CreatePermission' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CreatePermission' {} a -> s {certificateAuthorityArn = a} :: CreatePermission)

-- | The AWS service or identity that receives the permission. At this time,
-- the only valid principal is @acm.amazonaws.com@.
createPermission_principal :: Lens.Lens' CreatePermission Prelude.Text
createPermission_principal = Lens.lens (\CreatePermission' {principal} -> principal) (\s@CreatePermission' {} a -> s {principal = a} :: CreatePermission)

-- | The actions that the specified AWS service principal can use. These
-- include @IssueCertificate@, @GetCertificate@, and @ListPermissions@.
createPermission_actions :: Lens.Lens' CreatePermission (Prelude.NonEmpty ActionType)
createPermission_actions = Lens.lens (\CreatePermission' {actions} -> actions) (\s@CreatePermission' {} a -> s {actions = a} :: CreatePermission) Prelude.. Lens._Coerce

instance Core.AWSRequest CreatePermission where
  type
    AWSResponse CreatePermission =
      CreatePermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull CreatePermissionResponse'

instance Prelude.Hashable CreatePermission

instance Prelude.NFData CreatePermission

instance Core.ToHeaders CreatePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.CreatePermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePermission where
  toJSON CreatePermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceAccount" Core..=) Prelude.<$> sourceAccount,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              ),
            Prelude.Just ("Principal" Core..= principal),
            Prelude.Just ("Actions" Core..= actions)
          ]
      )

instance Core.ToPath CreatePermission where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePermissionResponse' smart constructor.
data CreatePermissionResponse = CreatePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreatePermissionResponse ::
  CreatePermissionResponse
newCreatePermissionResponse =
  CreatePermissionResponse'

instance Prelude.NFData CreatePermissionResponse

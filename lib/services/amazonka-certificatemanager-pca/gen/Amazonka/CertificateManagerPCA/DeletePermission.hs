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
-- Module      : Amazonka.CertificateManagerPCA.DeletePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes permissions on a private CA granted to the Certificate Manager
-- (ACM) service principal (acm.amazonaws.com).
--
-- These permissions allow ACM to issue and renew ACM certificates that
-- reside in the same Amazon Web Services account as the CA. If you revoke
-- these permissions, ACM will no longer renew the affected certificates
-- automatically.
--
-- Permissions can be granted with the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreatePermission.html CreatePermission>
-- action and listed with the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListPermissions.html ListPermissions>
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
--     <https://docs.aws.amazon.com/privateca/latest/userguide/pca-rbp.html Using a Resource Based Policy with Amazon Web Services Private CA>.
module Amazonka.CertificateManagerPCA.DeletePermission
  ( -- * Creating a Request
    DeletePermission (..),
    newDeletePermission,

    -- * Request Lenses
    deletePermission_sourceAccount,
    deletePermission_certificateAuthorityArn,
    deletePermission_principal,

    -- * Destructuring the Response
    DeletePermissionResponse (..),
    newDeletePermissionResponse,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { -- | The Amazon Web Services account that calls this action.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the private CA that issued the
    -- permissions. You can find the CA\'s ARN by calling the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
    -- action. This must have the following form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
    certificateAuthorityArn :: Prelude.Text,
    -- | The Amazon Web Services service or identity that will have its CA
    -- permissions revoked. At this time, the only valid service principal is
    -- @acm.amazonaws.com@
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAccount', 'deletePermission_sourceAccount' - The Amazon Web Services account that calls this action.
--
-- 'certificateAuthorityArn', 'deletePermission_certificateAuthorityArn' - The Amazon Resource Number (ARN) of the private CA that issued the
-- permissions. You can find the CA\'s ARN by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. This must have the following form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
--
-- 'principal', 'deletePermission_principal' - The Amazon Web Services service or identity that will have its CA
-- permissions revoked. At this time, the only valid service principal is
-- @acm.amazonaws.com@
newDeletePermission ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  DeletePermission
newDeletePermission
  pCertificateAuthorityArn_
  pPrincipal_ =
    DeletePermission'
      { sourceAccount = Prelude.Nothing,
        certificateAuthorityArn = pCertificateAuthorityArn_,
        principal = pPrincipal_
      }

-- | The Amazon Web Services account that calls this action.
deletePermission_sourceAccount :: Lens.Lens' DeletePermission (Prelude.Maybe Prelude.Text)
deletePermission_sourceAccount = Lens.lens (\DeletePermission' {sourceAccount} -> sourceAccount) (\s@DeletePermission' {} a -> s {sourceAccount = a} :: DeletePermission)

-- | The Amazon Resource Number (ARN) of the private CA that issued the
-- permissions. You can find the CA\'s ARN by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action. This must have the following form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
deletePermission_certificateAuthorityArn :: Lens.Lens' DeletePermission Prelude.Text
deletePermission_certificateAuthorityArn = Lens.lens (\DeletePermission' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@DeletePermission' {} a -> s {certificateAuthorityArn = a} :: DeletePermission)

-- | The Amazon Web Services service or identity that will have its CA
-- permissions revoked. At this time, the only valid service principal is
-- @acm.amazonaws.com@
deletePermission_principal :: Lens.Lens' DeletePermission Prelude.Text
deletePermission_principal = Lens.lens (\DeletePermission' {principal} -> principal) (\s@DeletePermission' {} a -> s {principal = a} :: DeletePermission)

instance Core.AWSRequest DeletePermission where
  type
    AWSResponse DeletePermission =
      DeletePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeletePermissionResponse'

instance Prelude.Hashable DeletePermission where
  hashWithSalt _salt DeletePermission' {..} =
    _salt
      `Prelude.hashWithSalt` sourceAccount
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` principal

instance Prelude.NFData DeletePermission where
  rnf DeletePermission' {..} =
    Prelude.rnf sourceAccount `Prelude.seq`
      Prelude.rnf certificateAuthorityArn `Prelude.seq`
        Prelude.rnf principal

instance Data.ToHeaders DeletePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.DeletePermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePermission where
  toJSON DeletePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceAccount" Data..=) Prelude.<$> sourceAccount,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              ),
            Prelude.Just ("Principal" Data..= principal)
          ]
      )

instance Data.ToPath DeletePermission where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePermissionResponse' smart constructor.
data DeletePermissionResponse = DeletePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePermissionResponse ::
  DeletePermissionResponse
newDeletePermissionResponse =
  DeletePermissionResponse'

instance Prelude.NFData DeletePermissionResponse where
  rnf _ = ()

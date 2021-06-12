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
-- Module      : Network.AWS.CertificateManager.DeleteCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a certificate and its associated private key. If this action
-- succeeds, the certificate no longer appears in the list that can be
-- displayed by calling the ListCertificates action or be retrieved by
-- calling the GetCertificate action. The certificate will not be available
-- for use by AWS services integrated with ACM.
--
-- You cannot delete an ACM certificate that is being used by another AWS
-- service. To delete a certificate that is in use, the certificate
-- association must first be removed.
module Network.AWS.CertificateManager.DeleteCertificate
  ( -- * Creating a Request
    DeleteCertificate (..),
    newDeleteCertificate,

    -- * Request Lenses
    deleteCertificate_certificateArn,

    -- * Destructuring the Response
    DeleteCertificateResponse (..),
    newDeleteCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { -- | String that contains the ARN of the ACM certificate to be deleted. This
    -- must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'deleteCertificate_certificateArn' - String that contains the ARN of the ACM certificate to be deleted. This
-- must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
newDeleteCertificate ::
  -- | 'certificateArn'
  Core.Text ->
  DeleteCertificate
newDeleteCertificate pCertificateArn_ =
  DeleteCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | String that contains the ARN of the ACM certificate to be deleted. This
-- must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
deleteCertificate_certificateArn :: Lens.Lens' DeleteCertificate Core.Text
deleteCertificate_certificateArn = Lens.lens (\DeleteCertificate' {certificateArn} -> certificateArn) (\s@DeleteCertificate' {} a -> s {certificateArn = a} :: DeleteCertificate)

instance Core.AWSRequest DeleteCertificate where
  type
    AWSResponse DeleteCertificate =
      DeleteCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteCertificateResponse'

instance Core.Hashable DeleteCertificate

instance Core.NFData DeleteCertificate

instance Core.ToHeaders DeleteCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.DeleteCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath DeleteCertificate where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCertificate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCertificateResponse ::
  DeleteCertificateResponse
newDeleteCertificateResponse =
  DeleteCertificateResponse'

instance Core.NFData DeleteCertificateResponse

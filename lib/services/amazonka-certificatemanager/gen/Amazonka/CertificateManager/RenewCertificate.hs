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
-- Module      : Amazonka.CertificateManager.RenewCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renews an eligible ACM certificate. At this time, only exported private
-- certificates can be renewed with this operation. In order to renew your
-- Amazon Web Services Private CA certificates with ACM, you must first
-- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaPermissions.html grant the ACM service principal permission to do so>.
-- For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/manual-renewal.html Testing Managed Renewal>
-- in the ACM User Guide.
module Amazonka.CertificateManager.RenewCertificate
  ( -- * Creating a Request
    RenewCertificate (..),
    newRenewCertificate,

    -- * Request Lenses
    renewCertificate_certificateArn,

    -- * Destructuring the Response
    RenewCertificateResponse (..),
    newRenewCertificateResponse,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRenewCertificate' smart constructor.
data RenewCertificate = RenewCertificate'
  { -- | String that contains the ARN of the ACM certificate to be renewed. This
    -- must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'renewCertificate_certificateArn' - String that contains the ARN of the ACM certificate to be renewed. This
-- must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
newRenewCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  RenewCertificate
newRenewCertificate pCertificateArn_ =
  RenewCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | String that contains the ARN of the ACM certificate to be renewed. This
-- must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
renewCertificate_certificateArn :: Lens.Lens' RenewCertificate Prelude.Text
renewCertificate_certificateArn = Lens.lens (\RenewCertificate' {certificateArn} -> certificateArn) (\s@RenewCertificate' {} a -> s {certificateArn = a} :: RenewCertificate)

instance Core.AWSRequest RenewCertificate where
  type
    AWSResponse RenewCertificate =
      RenewCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RenewCertificateResponse'

instance Prelude.Hashable RenewCertificate where
  hashWithSalt _salt RenewCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData RenewCertificate where
  rnf RenewCertificate' {..} =
    Prelude.rnf certificateArn

instance Data.ToHeaders RenewCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.RenewCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RenewCertificate where
  toJSON RenewCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Data..= certificateArn)
          ]
      )

instance Data.ToPath RenewCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery RenewCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRenewCertificateResponse' smart constructor.
data RenewCertificateResponse = RenewCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRenewCertificateResponse ::
  RenewCertificateResponse
newRenewCertificateResponse =
  RenewCertificateResponse'

instance Prelude.NFData RenewCertificateResponse where
  rnf _ = ()

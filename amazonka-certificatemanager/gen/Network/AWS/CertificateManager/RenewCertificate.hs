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
-- Module      : Network.AWS.CertificateManager.RenewCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renews an eligible ACM certificate. At this time, only exported private
-- certificates can be renewed with this operation. In order to renew your
-- ACM PCA certificates with ACM, you must first
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaPermissions.html grant the ACM service principal permission to do so>.
-- For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/manual-renewal.html Testing Managed Renewal>
-- in the ACM User Guide.
module Network.AWS.CertificateManager.RenewCertificate
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

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest RenewCertificate where
  type Rs RenewCertificate = RenewCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RenewCertificateResponse'

instance Prelude.Hashable RenewCertificate

instance Prelude.NFData RenewCertificate

instance Prelude.ToHeaders RenewCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CertificateManager.RenewCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RenewCertificate where
  toJSON RenewCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Prelude..= certificateArn)
          ]
      )

instance Prelude.ToPath RenewCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RenewCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRenewCertificateResponse' smart constructor.
data RenewCertificateResponse = RenewCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RenewCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRenewCertificateResponse ::
  RenewCertificateResponse
newRenewCertificateResponse =
  RenewCertificateResponse'

instance Prelude.NFData RenewCertificateResponse

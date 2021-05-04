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
-- Module      : Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a certificate authority (CA) that is in the @DELETED@ state.
-- You can restore a CA during the period that you defined in the
-- __PermanentDeletionTimeInDays__ parameter of the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeleteCertificateAuthority.html DeleteCertificateAuthority>
-- action. Currently, you can specify 7 to 30 days. If you did not specify
-- a __PermanentDeletionTimeInDays__ value, by default you can restore the
-- CA at any time in a 30 day period. You can check the time remaining in
-- the restoration period of a private CA in the @DELETED@ state by calling
-- the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DescribeCertificateAuthority.html DescribeCertificateAuthority>
-- or
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- actions. The status of a restored CA is set to its pre-deletion status
-- when the __RestoreCertificateAuthority__ action returns. To change its
-- status to @ACTIVE@, call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority>
-- action. If the private CA was in the @PENDING_CERTIFICATE@ state at
-- deletion, you must use the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate>
-- action to import a certificate authority into the private CA before it
-- can be activated. You cannot restore a CA after the restoration period
-- has ended.
module Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
  ( -- * Creating a Request
    RestoreCertificateAuthority (..),
    newRestoreCertificateAuthority,

    -- * Request Lenses
    restoreCertificateAuthority_certificateAuthorityArn,

    -- * Destructuring the Response
    RestoreCertificateAuthorityResponse (..),
    newRestoreCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRestoreCertificateAuthority' smart constructor.
data RestoreCertificateAuthority = RestoreCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
    -- action. This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'restoreCertificateAuthority_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
newRestoreCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  RestoreCertificateAuthority
newRestoreCertificateAuthority
  pCertificateAuthorityArn_ =
    RestoreCertificateAuthority'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
restoreCertificateAuthority_certificateAuthorityArn :: Lens.Lens' RestoreCertificateAuthority Prelude.Text
restoreCertificateAuthority_certificateAuthorityArn = Lens.lens (\RestoreCertificateAuthority' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@RestoreCertificateAuthority' {} a -> s {certificateAuthorityArn = a} :: RestoreCertificateAuthority)

instance
  Prelude.AWSRequest
    RestoreCertificateAuthority
  where
  type
    Rs RestoreCertificateAuthority =
      RestoreCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RestoreCertificateAuthorityResponse'

instance Prelude.Hashable RestoreCertificateAuthority

instance Prelude.NFData RestoreCertificateAuthority

instance
  Prelude.ToHeaders
    RestoreCertificateAuthority
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.RestoreCertificateAuthority" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RestoreCertificateAuthority where
  toJSON RestoreCertificateAuthority' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              )
          ]
      )

instance Prelude.ToPath RestoreCertificateAuthority where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RestoreCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreCertificateAuthorityResponse' smart constructor.
data RestoreCertificateAuthorityResponse = RestoreCertificateAuthorityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRestoreCertificateAuthorityResponse ::
  RestoreCertificateAuthorityResponse
newRestoreCertificateAuthorityResponse =
  RestoreCertificateAuthorityResponse'

instance
  Prelude.NFData
    RestoreCertificateAuthorityResponse

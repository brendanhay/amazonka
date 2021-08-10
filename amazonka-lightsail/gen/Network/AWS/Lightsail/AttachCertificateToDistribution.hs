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
-- Module      : Network.AWS.Lightsail.AttachCertificateToDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an SSL\/TLS certificate to your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- After the certificate is attached, your distribution accepts HTTPS
-- traffic for all of the domains that are associated with the certificate.
--
-- Use the @CreateCertificate@ action to create a certificate that you can
-- attach to your distribution.
--
-- Only certificates created in the @us-east-1@ AWS Region can be attached
-- to Lightsail distributions. Lightsail distributions are global resources
-- that can reference an origin in any AWS Region, and distribute its
-- content globally. However, all distributions are located in the
-- @us-east-1@ Region.
module Network.AWS.Lightsail.AttachCertificateToDistribution
  ( -- * Creating a Request
    AttachCertificateToDistribution (..),
    newAttachCertificateToDistribution,

    -- * Request Lenses
    attachCertificateToDistribution_distributionName,
    attachCertificateToDistribution_certificateName,

    -- * Destructuring the Response
    AttachCertificateToDistributionResponse (..),
    newAttachCertificateToDistributionResponse,

    -- * Response Lenses
    attachCertificateToDistributionResponse_operation,
    attachCertificateToDistributionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachCertificateToDistribution' smart constructor.
data AttachCertificateToDistribution = AttachCertificateToDistribution'
  { -- | The name of the distribution that the certificate will be attached to.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Text,
    -- | The name of the certificate to attach to a distribution.
    --
    -- Only certificates with a status of @ISSUED@ can be attached to a
    -- distribution.
    --
    -- Use the @GetCertificates@ action to get a list of certificate names that
    -- you can specify.
    --
    -- This is the name of the certificate resource type and is used only to
    -- reference the certificate in other API actions. It can be different than
    -- the domain name of the certificate. For example, your certificate name
    -- might be @WordPress-Blog-Certificate@ and the domain name of the
    -- certificate might be @example.com@.
    certificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachCertificateToDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'attachCertificateToDistribution_distributionName' - The name of the distribution that the certificate will be attached to.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- 'certificateName', 'attachCertificateToDistribution_certificateName' - The name of the certificate to attach to a distribution.
--
-- Only certificates with a status of @ISSUED@ can be attached to a
-- distribution.
--
-- Use the @GetCertificates@ action to get a list of certificate names that
-- you can specify.
--
-- This is the name of the certificate resource type and is used only to
-- reference the certificate in other API actions. It can be different than
-- the domain name of the certificate. For example, your certificate name
-- might be @WordPress-Blog-Certificate@ and the domain name of the
-- certificate might be @example.com@.
newAttachCertificateToDistribution ::
  -- | 'distributionName'
  Prelude.Text ->
  -- | 'certificateName'
  Prelude.Text ->
  AttachCertificateToDistribution
newAttachCertificateToDistribution
  pDistributionName_
  pCertificateName_ =
    AttachCertificateToDistribution'
      { distributionName =
          pDistributionName_,
        certificateName = pCertificateName_
      }

-- | The name of the distribution that the certificate will be attached to.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
attachCertificateToDistribution_distributionName :: Lens.Lens' AttachCertificateToDistribution Prelude.Text
attachCertificateToDistribution_distributionName = Lens.lens (\AttachCertificateToDistribution' {distributionName} -> distributionName) (\s@AttachCertificateToDistribution' {} a -> s {distributionName = a} :: AttachCertificateToDistribution)

-- | The name of the certificate to attach to a distribution.
--
-- Only certificates with a status of @ISSUED@ can be attached to a
-- distribution.
--
-- Use the @GetCertificates@ action to get a list of certificate names that
-- you can specify.
--
-- This is the name of the certificate resource type and is used only to
-- reference the certificate in other API actions. It can be different than
-- the domain name of the certificate. For example, your certificate name
-- might be @WordPress-Blog-Certificate@ and the domain name of the
-- certificate might be @example.com@.
attachCertificateToDistribution_certificateName :: Lens.Lens' AttachCertificateToDistribution Prelude.Text
attachCertificateToDistribution_certificateName = Lens.lens (\AttachCertificateToDistribution' {certificateName} -> certificateName) (\s@AttachCertificateToDistribution' {} a -> s {certificateName = a} :: AttachCertificateToDistribution)

instance
  Core.AWSRequest
    AttachCertificateToDistribution
  where
  type
    AWSResponse AttachCertificateToDistribution =
      AttachCertificateToDistributionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachCertificateToDistributionResponse'
            Prelude.<$> (x Core..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachCertificateToDistribution

instance
  Prelude.NFData
    AttachCertificateToDistribution

instance
  Core.ToHeaders
    AttachCertificateToDistribution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.AttachCertificateToDistribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AttachCertificateToDistribution where
  toJSON AttachCertificateToDistribution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("distributionName" Core..= distributionName),
            Prelude.Just
              ("certificateName" Core..= certificateName)
          ]
      )

instance Core.ToPath AttachCertificateToDistribution where
  toPath = Prelude.const "/"

instance Core.ToQuery AttachCertificateToDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachCertificateToDistributionResponse' smart constructor.
data AttachCertificateToDistributionResponse = AttachCertificateToDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachCertificateToDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'attachCertificateToDistributionResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'attachCertificateToDistributionResponse_httpStatus' - The response's http status code.
newAttachCertificateToDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachCertificateToDistributionResponse
newAttachCertificateToDistributionResponse
  pHttpStatus_ =
    AttachCertificateToDistributionResponse'
      { operation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
attachCertificateToDistributionResponse_operation :: Lens.Lens' AttachCertificateToDistributionResponse (Prelude.Maybe Operation)
attachCertificateToDistributionResponse_operation = Lens.lens (\AttachCertificateToDistributionResponse' {operation} -> operation) (\s@AttachCertificateToDistributionResponse' {} a -> s {operation = a} :: AttachCertificateToDistributionResponse)

-- | The response's http status code.
attachCertificateToDistributionResponse_httpStatus :: Lens.Lens' AttachCertificateToDistributionResponse Prelude.Int
attachCertificateToDistributionResponse_httpStatus = Lens.lens (\AttachCertificateToDistributionResponse' {httpStatus} -> httpStatus) (\s@AttachCertificateToDistributionResponse' {} a -> s {httpStatus = a} :: AttachCertificateToDistributionResponse)

instance
  Prelude.NFData
    AttachCertificateToDistributionResponse

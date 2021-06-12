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
-- Module      : Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSL\/TLS certificate for an Amazon Lightsail load balancer.
--
-- TLS is just an updated, more secure version of Secure Socket Layer
-- (SSL).
--
-- The @CreateLoadBalancerTlsCertificate@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
  ( -- * Creating a Request
    CreateLoadBalancerTlsCertificate (..),
    newCreateLoadBalancerTlsCertificate,

    -- * Request Lenses
    createLoadBalancerTlsCertificate_certificateAlternativeNames,
    createLoadBalancerTlsCertificate_tags,
    createLoadBalancerTlsCertificate_loadBalancerName,
    createLoadBalancerTlsCertificate_certificateName,
    createLoadBalancerTlsCertificate_certificateDomainName,

    -- * Destructuring the Response
    CreateLoadBalancerTlsCertificateResponse (..),
    newCreateLoadBalancerTlsCertificateResponse,

    -- * Response Lenses
    createLoadBalancerTlsCertificateResponse_operations,
    createLoadBalancerTlsCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoadBalancerTlsCertificate' smart constructor.
data CreateLoadBalancerTlsCertificate = CreateLoadBalancerTlsCertificate'
  { -- | An array of strings listing alternative domains and subdomains for your
    -- SSL\/TLS certificate. Lightsail will de-dupe the names for you. You can
    -- have a maximum of 9 alternative names (in addition to the 1 primary
    -- domain). We do not support wildcards (e.g., @*.example.com@).
    certificateAlternativeNames :: Core.Maybe [Core.Text],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The load balancer name where you want to create the SSL\/TLS
    -- certificate.
    loadBalancerName :: Core.Text,
    -- | The SSL\/TLS certificate name.
    --
    -- You can have up to 10 certificates in your account at one time. Each
    -- Lightsail load balancer can have up to 2 certificates associated with it
    -- at one time. There is also an overall limit to the number of
    -- certificates that can be issue in a 365-day period. For more
    -- information, see
    -- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>.
    certificateName :: Core.Text,
    -- | The domain name (e.g., @example.com@) for your SSL\/TLS certificate.
    certificateDomainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAlternativeNames', 'createLoadBalancerTlsCertificate_certificateAlternativeNames' - An array of strings listing alternative domains and subdomains for your
-- SSL\/TLS certificate. Lightsail will de-dupe the names for you. You can
-- have a maximum of 9 alternative names (in addition to the 1 primary
-- domain). We do not support wildcards (e.g., @*.example.com@).
--
-- 'tags', 'createLoadBalancerTlsCertificate_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'loadBalancerName', 'createLoadBalancerTlsCertificate_loadBalancerName' - The load balancer name where you want to create the SSL\/TLS
-- certificate.
--
-- 'certificateName', 'createLoadBalancerTlsCertificate_certificateName' - The SSL\/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each
-- Lightsail load balancer can have up to 2 certificates associated with it
-- at one time. There is also an overall limit to the number of
-- certificates that can be issue in a 365-day period. For more
-- information, see
-- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>.
--
-- 'certificateDomainName', 'createLoadBalancerTlsCertificate_certificateDomainName' - The domain name (e.g., @example.com@) for your SSL\/TLS certificate.
newCreateLoadBalancerTlsCertificate ::
  -- | 'loadBalancerName'
  Core.Text ->
  -- | 'certificateName'
  Core.Text ->
  -- | 'certificateDomainName'
  Core.Text ->
  CreateLoadBalancerTlsCertificate
newCreateLoadBalancerTlsCertificate
  pLoadBalancerName_
  pCertificateName_
  pCertificateDomainName_ =
    CreateLoadBalancerTlsCertificate'
      { certificateAlternativeNames =
          Core.Nothing,
        tags = Core.Nothing,
        loadBalancerName = pLoadBalancerName_,
        certificateName = pCertificateName_,
        certificateDomainName =
          pCertificateDomainName_
      }

-- | An array of strings listing alternative domains and subdomains for your
-- SSL\/TLS certificate. Lightsail will de-dupe the names for you. You can
-- have a maximum of 9 alternative names (in addition to the 1 primary
-- domain). We do not support wildcards (e.g., @*.example.com@).
createLoadBalancerTlsCertificate_certificateAlternativeNames :: Lens.Lens' CreateLoadBalancerTlsCertificate (Core.Maybe [Core.Text])
createLoadBalancerTlsCertificate_certificateAlternativeNames = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateAlternativeNames} -> certificateAlternativeNames) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateAlternativeNames = a} :: CreateLoadBalancerTlsCertificate) Core.. Lens.mapping Lens._Coerce

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createLoadBalancerTlsCertificate_tags :: Lens.Lens' CreateLoadBalancerTlsCertificate (Core.Maybe [Tag])
createLoadBalancerTlsCertificate_tags = Lens.lens (\CreateLoadBalancerTlsCertificate' {tags} -> tags) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {tags = a} :: CreateLoadBalancerTlsCertificate) Core.. Lens.mapping Lens._Coerce

-- | The load balancer name where you want to create the SSL\/TLS
-- certificate.
createLoadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' CreateLoadBalancerTlsCertificate Core.Text
createLoadBalancerTlsCertificate_loadBalancerName = Lens.lens (\CreateLoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: CreateLoadBalancerTlsCertificate)

-- | The SSL\/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each
-- Lightsail load balancer can have up to 2 certificates associated with it
-- at one time. There is also an overall limit to the number of
-- certificates that can be issue in a 365-day period. For more
-- information, see
-- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>.
createLoadBalancerTlsCertificate_certificateName :: Lens.Lens' CreateLoadBalancerTlsCertificate Core.Text
createLoadBalancerTlsCertificate_certificateName = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateName} -> certificateName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateName = a} :: CreateLoadBalancerTlsCertificate)

-- | The domain name (e.g., @example.com@) for your SSL\/TLS certificate.
createLoadBalancerTlsCertificate_certificateDomainName :: Lens.Lens' CreateLoadBalancerTlsCertificate Core.Text
createLoadBalancerTlsCertificate_certificateDomainName = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateDomainName} -> certificateDomainName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateDomainName = a} :: CreateLoadBalancerTlsCertificate)

instance
  Core.AWSRequest
    CreateLoadBalancerTlsCertificate
  where
  type
    AWSResponse CreateLoadBalancerTlsCertificate =
      CreateLoadBalancerTlsCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoadBalancerTlsCertificateResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateLoadBalancerTlsCertificate

instance Core.NFData CreateLoadBalancerTlsCertificate

instance
  Core.ToHeaders
    CreateLoadBalancerTlsCertificate
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateLoadBalancerTlsCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateLoadBalancerTlsCertificate where
  toJSON CreateLoadBalancerTlsCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("certificateAlternativeNames" Core..=)
              Core.<$> certificateAlternativeNames,
            ("tags" Core..=) Core.<$> tags,
            Core.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Core.Just
              ("certificateName" Core..= certificateName),
            Core.Just
              ( "certificateDomainName"
                  Core..= certificateDomainName
              )
          ]
      )

instance Core.ToPath CreateLoadBalancerTlsCertificate where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateLoadBalancerTlsCertificate
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateLoadBalancerTlsCertificateResponse' smart constructor.
data CreateLoadBalancerTlsCertificateResponse = CreateLoadBalancerTlsCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancerTlsCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createLoadBalancerTlsCertificateResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createLoadBalancerTlsCertificateResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerTlsCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLoadBalancerTlsCertificateResponse
newCreateLoadBalancerTlsCertificateResponse
  pHttpStatus_ =
    CreateLoadBalancerTlsCertificateResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createLoadBalancerTlsCertificateResponse_operations :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse (Core.Maybe [Operation])
createLoadBalancerTlsCertificateResponse_operations = Lens.lens (\CreateLoadBalancerTlsCertificateResponse' {operations} -> operations) (\s@CreateLoadBalancerTlsCertificateResponse' {} a -> s {operations = a} :: CreateLoadBalancerTlsCertificateResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createLoadBalancerTlsCertificateResponse_httpStatus :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse Core.Int
createLoadBalancerTlsCertificateResponse_httpStatus = Lens.lens (\CreateLoadBalancerTlsCertificateResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerTlsCertificateResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerTlsCertificateResponse)

instance
  Core.NFData
    CreateLoadBalancerTlsCertificateResponse

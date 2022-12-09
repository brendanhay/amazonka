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
-- Module      : Amazonka.Lightsail.CreateLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateLoadBalancerTlsCertificate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLoadBalancerTlsCertificate' smart constructor.
data CreateLoadBalancerTlsCertificate = CreateLoadBalancerTlsCertificate'
  { -- | An array of strings listing alternative domains and subdomains for your
    -- SSL\/TLS certificate. Lightsail will de-dupe the names for you. You can
    -- have a maximum of 9 alternative names (in addition to the 1 primary
    -- domain). We do not support wildcards (e.g., @*.example.com@).
    certificateAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The load balancer name where you want to create the SSL\/TLS
    -- certificate.
    loadBalancerName :: Prelude.Text,
    -- | The SSL\/TLS certificate name.
    --
    -- You can have up to 10 certificates in your account at one time. Each
    -- Lightsail load balancer can have up to 2 certificates associated with it
    -- at one time. There is also an overall limit to the number of
    -- certificates that can be issue in a 365-day period. For more
    -- information, see
    -- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>.
    certificateName :: Prelude.Text,
    -- | The domain name (e.g., @example.com@) for your SSL\/TLS certificate.
    certificateDomainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'certificateName'
  Prelude.Text ->
  -- | 'certificateDomainName'
  Prelude.Text ->
  CreateLoadBalancerTlsCertificate
newCreateLoadBalancerTlsCertificate
  pLoadBalancerName_
  pCertificateName_
  pCertificateDomainName_ =
    CreateLoadBalancerTlsCertificate'
      { certificateAlternativeNames =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        certificateName = pCertificateName_,
        certificateDomainName =
          pCertificateDomainName_
      }

-- | An array of strings listing alternative domains and subdomains for your
-- SSL\/TLS certificate. Lightsail will de-dupe the names for you. You can
-- have a maximum of 9 alternative names (in addition to the 1 primary
-- domain). We do not support wildcards (e.g., @*.example.com@).
createLoadBalancerTlsCertificate_certificateAlternativeNames :: Lens.Lens' CreateLoadBalancerTlsCertificate (Prelude.Maybe [Prelude.Text])
createLoadBalancerTlsCertificate_certificateAlternativeNames = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateAlternativeNames} -> certificateAlternativeNames) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateAlternativeNames = a} :: CreateLoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createLoadBalancerTlsCertificate_tags :: Lens.Lens' CreateLoadBalancerTlsCertificate (Prelude.Maybe [Tag])
createLoadBalancerTlsCertificate_tags = Lens.lens (\CreateLoadBalancerTlsCertificate' {tags} -> tags) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {tags = a} :: CreateLoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The load balancer name where you want to create the SSL\/TLS
-- certificate.
createLoadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' CreateLoadBalancerTlsCertificate Prelude.Text
createLoadBalancerTlsCertificate_loadBalancerName = Lens.lens (\CreateLoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: CreateLoadBalancerTlsCertificate)

-- | The SSL\/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each
-- Lightsail load balancer can have up to 2 certificates associated with it
-- at one time. There is also an overall limit to the number of
-- certificates that can be issue in a 365-day period. For more
-- information, see
-- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>.
createLoadBalancerTlsCertificate_certificateName :: Lens.Lens' CreateLoadBalancerTlsCertificate Prelude.Text
createLoadBalancerTlsCertificate_certificateName = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateName} -> certificateName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateName = a} :: CreateLoadBalancerTlsCertificate)

-- | The domain name (e.g., @example.com@) for your SSL\/TLS certificate.
createLoadBalancerTlsCertificate_certificateDomainName :: Lens.Lens' CreateLoadBalancerTlsCertificate Prelude.Text
createLoadBalancerTlsCertificate_certificateDomainName = Lens.lens (\CreateLoadBalancerTlsCertificate' {certificateDomainName} -> certificateDomainName) (\s@CreateLoadBalancerTlsCertificate' {} a -> s {certificateDomainName = a} :: CreateLoadBalancerTlsCertificate)

instance
  Core.AWSRequest
    CreateLoadBalancerTlsCertificate
  where
  type
    AWSResponse CreateLoadBalancerTlsCertificate =
      CreateLoadBalancerTlsCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoadBalancerTlsCertificateResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLoadBalancerTlsCertificate
  where
  hashWithSalt
    _salt
    CreateLoadBalancerTlsCertificate' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAlternativeNames
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` certificateName
        `Prelude.hashWithSalt` certificateDomainName

instance
  Prelude.NFData
    CreateLoadBalancerTlsCertificate
  where
  rnf CreateLoadBalancerTlsCertificate' {..} =
    Prelude.rnf certificateAlternativeNames
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf certificateDomainName

instance
  Data.ToHeaders
    CreateLoadBalancerTlsCertificate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateLoadBalancerTlsCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLoadBalancerTlsCertificate where
  toJSON CreateLoadBalancerTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateAlternativeNames" Data..=)
              Prelude.<$> certificateAlternativeNames,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName),
            Prelude.Just
              ("certificateName" Data..= certificateName),
            Prelude.Just
              ( "certificateDomainName"
                  Data..= certificateDomainName
              )
          ]
      )

instance Data.ToPath CreateLoadBalancerTlsCertificate where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateLoadBalancerTlsCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLoadBalancerTlsCertificateResponse' smart constructor.
data CreateLoadBalancerTlsCertificateResponse = CreateLoadBalancerTlsCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateLoadBalancerTlsCertificateResponse
newCreateLoadBalancerTlsCertificateResponse
  pHttpStatus_ =
    CreateLoadBalancerTlsCertificateResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createLoadBalancerTlsCertificateResponse_operations :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse (Prelude.Maybe [Operation])
createLoadBalancerTlsCertificateResponse_operations = Lens.lens (\CreateLoadBalancerTlsCertificateResponse' {operations} -> operations) (\s@CreateLoadBalancerTlsCertificateResponse' {} a -> s {operations = a} :: CreateLoadBalancerTlsCertificateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLoadBalancerTlsCertificateResponse_httpStatus :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse Prelude.Int
createLoadBalancerTlsCertificateResponse_httpStatus = Lens.lens (\CreateLoadBalancerTlsCertificateResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerTlsCertificateResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerTlsCertificateResponse)

instance
  Prelude.NFData
    CreateLoadBalancerTlsCertificateResponse
  where
  rnf CreateLoadBalancerTlsCertificateResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus

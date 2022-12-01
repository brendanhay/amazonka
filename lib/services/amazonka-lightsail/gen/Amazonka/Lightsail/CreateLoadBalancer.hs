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
-- Module      : Amazonka.Lightsail.CreateLoadBalancer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer. To learn more about deciding whether
-- to load balance your application, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/configure-lightsail-instances-for-load-balancing Configure your Lightsail instances for load balancing>.
-- You can create up to 5 load balancers per AWS Region in your account.
--
-- When you create a load balancer, you can specify a unique name and port
-- settings. To change additional load balancer settings, use the
-- @UpdateLoadBalancerAttribute@ operation.
--
-- The @create load balancer@ operation supports tag-based access control
-- via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateLoadBalancer
  ( -- * Creating a Request
    CreateLoadBalancer (..),
    newCreateLoadBalancer,

    -- * Request Lenses
    createLoadBalancer_tags,
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_certificateName,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_tlsPolicyName,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_instancePort,

    -- * Destructuring the Response
    CreateLoadBalancerResponse (..),
    newCreateLoadBalancerResponse,

    -- * Response Lenses
    createLoadBalancerResponse_operations,
    createLoadBalancerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The path you provided to perform the load balancer health check. If you
    -- didn\'t specify a health check path, Lightsail uses the root path of
    -- your website (e.g., @\"\/\"@).
    --
    -- You may want to specify a custom health check path other than the root
    -- of your application if your home page loads slowly or has a lot of media
    -- or scripting on it.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The optional alternative domains and subdomains to use with your
    -- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
    -- @m.example.com@, @blog.example.com@).
    certificateAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the SSL\/TLS certificate.
    --
    -- If you specify @certificateName@, then @certificateDomainName@ is
    -- required (and vice-versa).
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The IP address type for the load balancer.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    --
    -- The default value is @dualstack@.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The domain name with which your certificate is associated (e.g.,
    -- @example.com@).
    --
    -- If you specify @certificateDomainName@, then @certificateName@ is
    -- required (and vice-versa).
    certificateDomainName :: Prelude.Maybe Prelude.Text,
    -- | The name of the TLS policy to apply to the load balancer.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
    -- action to get a list of TLS policy names that you can specify.
    --
    -- For more information about load balancer TLS policies, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configure-load-balancer-tls-security-policy Configuring TLS security policies on your Amazon Lightsail load balancers>
    -- in the /Amazon Lightsail Developer Guide/.
    tlsPolicyName :: Prelude.Maybe Prelude.Text,
    -- | The name of your load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The instance port where you\'re creating your load balancer.
    instancePort :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLoadBalancer_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'healthCheckPath', 'createLoadBalancer_healthCheckPath' - The path you provided to perform the load balancer health check. If you
-- didn\'t specify a health check path, Lightsail uses the root path of
-- your website (e.g., @\"\/\"@).
--
-- You may want to specify a custom health check path other than the root
-- of your application if your home page loads slowly or has a lot of media
-- or scripting on it.
--
-- 'certificateAlternativeNames', 'createLoadBalancer_certificateAlternativeNames' - The optional alternative domains and subdomains to use with your
-- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
-- @m.example.com@, @blog.example.com@).
--
-- 'certificateName', 'createLoadBalancer_certificateName' - The name of the SSL\/TLS certificate.
--
-- If you specify @certificateName@, then @certificateDomainName@ is
-- required (and vice-versa).
--
-- 'ipAddressType', 'createLoadBalancer_ipAddressType' - The IP address type for the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
--
-- 'certificateDomainName', 'createLoadBalancer_certificateDomainName' - The domain name with which your certificate is associated (e.g.,
-- @example.com@).
--
-- If you specify @certificateDomainName@, then @certificateName@ is
-- required (and vice-versa).
--
-- 'tlsPolicyName', 'createLoadBalancer_tlsPolicyName' - The name of the TLS policy to apply to the load balancer.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
-- action to get a list of TLS policy names that you can specify.
--
-- For more information about load balancer TLS policies, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configure-load-balancer-tls-security-policy Configuring TLS security policies on your Amazon Lightsail load balancers>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'loadBalancerName', 'createLoadBalancer_loadBalancerName' - The name of your load balancer.
--
-- 'instancePort', 'createLoadBalancer_instancePort' - The instance port where you\'re creating your load balancer.
newCreateLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'instancePort'
  Prelude.Int ->
  CreateLoadBalancer
newCreateLoadBalancer
  pLoadBalancerName_
  pInstancePort_ =
    CreateLoadBalancer'
      { tags = Prelude.Nothing,
        healthCheckPath = Prelude.Nothing,
        certificateAlternativeNames = Prelude.Nothing,
        certificateName = Prelude.Nothing,
        ipAddressType = Prelude.Nothing,
        certificateDomainName = Prelude.Nothing,
        tlsPolicyName = Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        instancePort = pInstancePort_
      }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createLoadBalancer_tags :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Tag])
createLoadBalancer_tags = Lens.lens (\CreateLoadBalancer' {tags} -> tags) (\s@CreateLoadBalancer' {} a -> s {tags = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The path you provided to perform the load balancer health check. If you
-- didn\'t specify a health check path, Lightsail uses the root path of
-- your website (e.g., @\"\/\"@).
--
-- You may want to specify a custom health check path other than the root
-- of your application if your home page loads slowly or has a lot of media
-- or scripting on it.
createLoadBalancer_healthCheckPath :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_healthCheckPath = Lens.lens (\CreateLoadBalancer' {healthCheckPath} -> healthCheckPath) (\s@CreateLoadBalancer' {} a -> s {healthCheckPath = a} :: CreateLoadBalancer)

-- | The optional alternative domains and subdomains to use with your
-- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
-- @m.example.com@, @blog.example.com@).
createLoadBalancer_certificateAlternativeNames :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Prelude.Text])
createLoadBalancer_certificateAlternativeNames = Lens.lens (\CreateLoadBalancer' {certificateAlternativeNames} -> certificateAlternativeNames) (\s@CreateLoadBalancer' {} a -> s {certificateAlternativeNames = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The name of the SSL\/TLS certificate.
--
-- If you specify @certificateName@, then @certificateDomainName@ is
-- required (and vice-versa).
createLoadBalancer_certificateName :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_certificateName = Lens.lens (\CreateLoadBalancer' {certificateName} -> certificateName) (\s@CreateLoadBalancer' {} a -> s {certificateName = a} :: CreateLoadBalancer)

-- | The IP address type for the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
createLoadBalancer_ipAddressType :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe IpAddressType)
createLoadBalancer_ipAddressType = Lens.lens (\CreateLoadBalancer' {ipAddressType} -> ipAddressType) (\s@CreateLoadBalancer' {} a -> s {ipAddressType = a} :: CreateLoadBalancer)

-- | The domain name with which your certificate is associated (e.g.,
-- @example.com@).
--
-- If you specify @certificateDomainName@, then @certificateName@ is
-- required (and vice-versa).
createLoadBalancer_certificateDomainName :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_certificateDomainName = Lens.lens (\CreateLoadBalancer' {certificateDomainName} -> certificateDomainName) (\s@CreateLoadBalancer' {} a -> s {certificateDomainName = a} :: CreateLoadBalancer)

-- | The name of the TLS policy to apply to the load balancer.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
-- action to get a list of TLS policy names that you can specify.
--
-- For more information about load balancer TLS policies, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configure-load-balancer-tls-security-policy Configuring TLS security policies on your Amazon Lightsail load balancers>
-- in the /Amazon Lightsail Developer Guide/.
createLoadBalancer_tlsPolicyName :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_tlsPolicyName = Lens.lens (\CreateLoadBalancer' {tlsPolicyName} -> tlsPolicyName) (\s@CreateLoadBalancer' {} a -> s {tlsPolicyName = a} :: CreateLoadBalancer)

-- | The name of your load balancer.
createLoadBalancer_loadBalancerName :: Lens.Lens' CreateLoadBalancer Prelude.Text
createLoadBalancer_loadBalancerName = Lens.lens (\CreateLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancer' {} a -> s {loadBalancerName = a} :: CreateLoadBalancer)

-- | The instance port where you\'re creating your load balancer.
createLoadBalancer_instancePort :: Lens.Lens' CreateLoadBalancer Prelude.Int
createLoadBalancer_instancePort = Lens.lens (\CreateLoadBalancer' {instancePort} -> instancePort) (\s@CreateLoadBalancer' {} a -> s {instancePort = a} :: CreateLoadBalancer)

instance Core.AWSRequest CreateLoadBalancer where
  type
    AWSResponse CreateLoadBalancer =
      CreateLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoadBalancerResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancer where
  hashWithSalt _salt CreateLoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` certificateAlternativeNames
      `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` certificateDomainName
      `Prelude.hashWithSalt` tlsPolicyName
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` instancePort

instance Prelude.NFData CreateLoadBalancer where
  rnf CreateLoadBalancer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf certificateAlternativeNames
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf certificateDomainName
      `Prelude.seq` Prelude.rnf tlsPolicyName
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf instancePort

instance Core.ToHeaders CreateLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLoadBalancer where
  toJSON CreateLoadBalancer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("healthCheckPath" Core..=)
              Prelude.<$> healthCheckPath,
            ("certificateAlternativeNames" Core..=)
              Prelude.<$> certificateAlternativeNames,
            ("certificateName" Core..=)
              Prelude.<$> certificateName,
            ("ipAddressType" Core..=) Prelude.<$> ipAddressType,
            ("certificateDomainName" Core..=)
              Prelude.<$> certificateDomainName,
            ("tlsPolicyName" Core..=) Prelude.<$> tlsPolicyName,
            Prelude.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Prelude.Just ("instancePort" Core..= instancePort)
          ]
      )

instance Core.ToPath CreateLoadBalancer where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createLoadBalancerResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createLoadBalancerResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoadBalancerResponse
newCreateLoadBalancerResponse pHttpStatus_ =
  CreateLoadBalancerResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createLoadBalancerResponse_operations :: Lens.Lens' CreateLoadBalancerResponse (Prelude.Maybe [Operation])
createLoadBalancerResponse_operations = Lens.lens (\CreateLoadBalancerResponse' {operations} -> operations) (\s@CreateLoadBalancerResponse' {} a -> s {operations = a} :: CreateLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLoadBalancerResponse_httpStatus :: Lens.Lens' CreateLoadBalancerResponse Prelude.Int
createLoadBalancerResponse_httpStatus = Lens.lens (\CreateLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerResponse)

instance Prelude.NFData CreateLoadBalancerResponse where
  rnf CreateLoadBalancerResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus

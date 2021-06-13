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
-- Module      : Network.AWS.Lightsail.CreateLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer. To learn more about deciding whether
-- to load balance your application, see
-- <https://lightsail.aws.amazon.com/ls/docs/how-to/article/configure-lightsail-instances-for-load-balancing Configure your Lightsail instances for load balancing>.
-- You can create up to 5 load balancers per AWS Region in your account.
--
-- When you create a load balancer, you can specify a unique name and port
-- settings. To change additional load balancer settings, use the
-- @UpdateLoadBalancerAttribute@ operation.
--
-- The @create load balancer@ operation supports tag-based access control
-- via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateLoadBalancer
  ( -- * Creating a Request
    CreateLoadBalancer (..),
    newCreateLoadBalancer,

    -- * Request Lenses
    createLoadBalancer_ipAddressType,
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_tags,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_certificateName,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | The IP address type for the load balancer.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    --
    -- The default value is @dualstack@.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The optional alternative domains and subdomains to use with your
    -- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
    -- @m.example.com@, @blog.example.com@).
    certificateAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The path you provided to perform the load balancer health check. If you
    -- didn\'t specify a health check path, Lightsail uses the root path of
    -- your website (e.g., @\"\/\"@).
    --
    -- You may want to specify a custom health check path other than the root
    -- of your application if your home page loads slowly or has a lot of media
    -- or scripting on it.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The domain name with which your certificate is associated (e.g.,
    -- @example.com@).
    --
    -- If you specify @certificateDomainName@, then @certificateName@ is
    -- required (and vice-versa).
    certificateDomainName :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSL\/TLS certificate.
    --
    -- If you specify @certificateName@, then @certificateDomainName@ is
    -- required (and vice-versa).
    certificateName :: Prelude.Maybe Prelude.Text,
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
-- 'ipAddressType', 'createLoadBalancer_ipAddressType' - The IP address type for the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
--
-- 'certificateAlternativeNames', 'createLoadBalancer_certificateAlternativeNames' - The optional alternative domains and subdomains to use with your
-- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
-- @m.example.com@, @blog.example.com@).
--
-- 'healthCheckPath', 'createLoadBalancer_healthCheckPath' - The path you provided to perform the load balancer health check. If you
-- didn\'t specify a health check path, Lightsail uses the root path of
-- your website (e.g., @\"\/\"@).
--
-- You may want to specify a custom health check path other than the root
-- of your application if your home page loads slowly or has a lot of media
-- or scripting on it.
--
-- 'tags', 'createLoadBalancer_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'certificateDomainName', 'createLoadBalancer_certificateDomainName' - The domain name with which your certificate is associated (e.g.,
-- @example.com@).
--
-- If you specify @certificateDomainName@, then @certificateName@ is
-- required (and vice-versa).
--
-- 'certificateName', 'createLoadBalancer_certificateName' - The name of the SSL\/TLS certificate.
--
-- If you specify @certificateName@, then @certificateDomainName@ is
-- required (and vice-versa).
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
      { ipAddressType =
          Prelude.Nothing,
        certificateAlternativeNames = Prelude.Nothing,
        healthCheckPath = Prelude.Nothing,
        tags = Prelude.Nothing,
        certificateDomainName = Prelude.Nothing,
        certificateName = Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        instancePort = pInstancePort_
      }

-- | The IP address type for the load balancer.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
createLoadBalancer_ipAddressType :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe IpAddressType)
createLoadBalancer_ipAddressType = Lens.lens (\CreateLoadBalancer' {ipAddressType} -> ipAddressType) (\s@CreateLoadBalancer' {} a -> s {ipAddressType = a} :: CreateLoadBalancer)

-- | The optional alternative domains and subdomains to use with your
-- SSL\/TLS certificate (e.g., @www.example.com@, @example.com@,
-- @m.example.com@, @blog.example.com@).
createLoadBalancer_certificateAlternativeNames :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Prelude.Text])
createLoadBalancer_certificateAlternativeNames = Lens.lens (\CreateLoadBalancer' {certificateAlternativeNames} -> certificateAlternativeNames) (\s@CreateLoadBalancer' {} a -> s {certificateAlternativeNames = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

-- | The path you provided to perform the load balancer health check. If you
-- didn\'t specify a health check path, Lightsail uses the root path of
-- your website (e.g., @\"\/\"@).
--
-- You may want to specify a custom health check path other than the root
-- of your application if your home page loads slowly or has a lot of media
-- or scripting on it.
createLoadBalancer_healthCheckPath :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_healthCheckPath = Lens.lens (\CreateLoadBalancer' {healthCheckPath} -> healthCheckPath) (\s@CreateLoadBalancer' {} a -> s {healthCheckPath = a} :: CreateLoadBalancer)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createLoadBalancer_tags :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Tag])
createLoadBalancer_tags = Lens.lens (\CreateLoadBalancer' {tags} -> tags) (\s@CreateLoadBalancer' {} a -> s {tags = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

-- | The domain name with which your certificate is associated (e.g.,
-- @example.com@).
--
-- If you specify @certificateDomainName@, then @certificateName@ is
-- required (and vice-versa).
createLoadBalancer_certificateDomainName :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_certificateDomainName = Lens.lens (\CreateLoadBalancer' {certificateDomainName} -> certificateDomainName) (\s@CreateLoadBalancer' {} a -> s {certificateDomainName = a} :: CreateLoadBalancer)

-- | The name of the SSL\/TLS certificate.
--
-- If you specify @certificateName@, then @certificateDomainName@ is
-- required (and vice-versa).
createLoadBalancer_certificateName :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_certificateName = Lens.lens (\CreateLoadBalancer' {certificateName} -> certificateName) (\s@CreateLoadBalancer' {} a -> s {certificateName = a} :: CreateLoadBalancer)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoadBalancerResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancer

instance Prelude.NFData CreateLoadBalancer

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
          [ ("ipAddressType" Core..=) Prelude.<$> ipAddressType,
            ("certificateAlternativeNames" Core..=)
              Prelude.<$> certificateAlternativeNames,
            ("healthCheckPath" Core..=)
              Prelude.<$> healthCheckPath,
            ("tags" Core..=) Prelude.<$> tags,
            ("certificateDomainName" Core..=)
              Prelude.<$> certificateDomainName,
            ("certificateName" Core..=)
              Prelude.<$> certificateName,
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
createLoadBalancerResponse_operations = Lens.lens (\CreateLoadBalancerResponse' {operations} -> operations) (\s@CreateLoadBalancerResponse' {} a -> s {operations = a} :: CreateLoadBalancerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createLoadBalancerResponse_httpStatus :: Lens.Lens' CreateLoadBalancerResponse Prelude.Int
createLoadBalancerResponse_httpStatus = Lens.lens (\CreateLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerResponse)

instance Prelude.NFData CreateLoadBalancerResponse

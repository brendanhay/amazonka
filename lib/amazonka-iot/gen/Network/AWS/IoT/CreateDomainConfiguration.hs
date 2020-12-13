{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain configuration.
module Network.AWS.IoT.CreateDomainConfiguration
  ( -- * Creating a request
    CreateDomainConfiguration (..),
    mkCreateDomainConfiguration,

    -- ** Request lenses
    cdcDomainConfigurationName,
    cdcAuthorizerConfig,
    cdcServerCertificateARNs,
    cdcDomainName,
    cdcServiceType,
    cdcValidationCertificateARN,
    cdcTags,

    -- * Destructuring the response
    CreateDomainConfigurationResponse (..),
    mkCreateDomainConfigurationResponse,

    -- ** Response lenses
    cdcrsDomainConfigurationName,
    cdcrsDomainConfigurationARN,
    cdcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDomainConfiguration' smart constructor.
data CreateDomainConfiguration = CreateDomainConfiguration'
  { -- | The name of the domain configuration. This value must be unique to a region.
    domainConfigurationName :: Lude.Text,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Lude.Maybe AuthorizerConfig,
    -- | The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
    serverCertificateARNs :: Lude.Maybe [Lude.Text],
    -- | The name of the domain.
    domainName :: Lude.Maybe Lude.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Lude.Maybe ServiceType,
    -- | The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
    validationCertificateARN :: Lude.Maybe Lude.Text,
    -- | Metadata which can be used to manage the domain configuration.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainConfiguration' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration. This value must be unique to a region.
-- * 'authorizerConfig' - An object that specifies the authorization service for a domain.
-- * 'serverCertificateARNs' - The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
-- * 'domainName' - The name of the domain.
-- * 'serviceType' - The type of service delivered by the endpoint.
-- * 'validationCertificateARN' - The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
-- * 'tags' - Metadata which can be used to manage the domain configuration.
mkCreateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Lude.Text ->
  CreateDomainConfiguration
mkCreateDomainConfiguration pDomainConfigurationName_ =
  CreateDomainConfiguration'
    { domainConfigurationName =
        pDomainConfigurationName_,
      authorizerConfig = Lude.Nothing,
      serverCertificateARNs = Lude.Nothing,
      domainName = Lude.Nothing,
      serviceType = Lude.Nothing,
      validationCertificateARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the domain configuration. This value must be unique to a region.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomainConfigurationName :: Lens.Lens' CreateDomainConfiguration Lude.Text
cdcDomainConfigurationName = Lens.lens (domainConfigurationName :: CreateDomainConfiguration -> Lude.Text) (\s a -> s {domainConfigurationName = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcAuthorizerConfig :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe AuthorizerConfig)
cdcAuthorizerConfig = Lens.lens (authorizerConfig :: CreateDomainConfiguration -> Lude.Maybe AuthorizerConfig) (\s a -> s {authorizerConfig = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The ARNs of the certificates that AWS IoT passes to the device during the TLS handshake. Currently you can specify only one certificate ARN. This value is not required for AWS-managed domains.
--
-- /Note:/ Consider using 'serverCertificateARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServerCertificateARNs :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe [Lude.Text])
cdcServerCertificateARNs = Lens.lens (serverCertificateARNs :: CreateDomainConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {serverCertificateARNs = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcServerCertificateARNs "Use generic-lens or generic-optics with 'serverCertificateARNs' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDomainName :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe Lude.Text)
cdcDomainName = Lens.lens (domainName :: CreateDomainConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServiceType :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe ServiceType)
cdcServiceType = Lens.lens (serviceType :: CreateDomainConfiguration -> Lude.Maybe ServiceType) (\s a -> s {serviceType = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The certificate used to validate the server certificate and prove domain name ownership. This certificate must be signed by a public certificate authority. This value is not required for AWS-managed domains.
--
-- /Note:/ Consider using 'validationCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcValidationCertificateARN :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe Lude.Text)
cdcValidationCertificateARN = Lens.lens (validationCertificateARN :: CreateDomainConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {validationCertificateARN = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcValidationCertificateARN "Use generic-lens or generic-optics with 'validationCertificateARN' instead." #-}

-- | Metadata which can be used to manage the domain configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDomainConfiguration (Lude.Maybe [Tag])
cdcTags = Lens.lens (tags :: CreateDomainConfiguration -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDomainConfiguration)
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDomainConfiguration where
  type
    Rs CreateDomainConfiguration =
      CreateDomainConfigurationResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDomainConfigurationResponse'
            Lude.<$> (x Lude..?> "domainConfigurationName")
            Lude.<*> (x Lude..?> "domainConfigurationArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDomainConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDomainConfiguration where
  toJSON CreateDomainConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authorizerConfig" Lude..=) Lude.<$> authorizerConfig,
            ("serverCertificateArns" Lude..=) Lude.<$> serverCertificateARNs,
            ("domainName" Lude..=) Lude.<$> domainName,
            ("serviceType" Lude..=) Lude.<$> serviceType,
            ("validationCertificateArn" Lude..=)
              Lude.<$> validationCertificateARN,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDomainConfiguration where
  toPath CreateDomainConfiguration' {..} =
    Lude.mconcat
      ["/domainConfigurations/", Lude.toBS domainConfigurationName]

instance Lude.ToQuery CreateDomainConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDomainConfigurationResponse' smart constructor.
data CreateDomainConfigurationResponse = CreateDomainConfigurationResponse'
  { -- | The name of the domain configuration.
    domainConfigurationName :: Lude.Maybe Lude.Text,
    -- | The ARN of the domain configuration.
    domainConfigurationARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration.
-- * 'domainConfigurationARN' - The ARN of the domain configuration.
-- * 'responseStatus' - The response status code.
mkCreateDomainConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDomainConfigurationResponse
mkCreateDomainConfigurationResponse pResponseStatus_ =
  CreateDomainConfigurationResponse'
    { domainConfigurationName =
        Lude.Nothing,
      domainConfigurationARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDomainConfigurationName :: Lens.Lens' CreateDomainConfigurationResponse (Lude.Maybe Lude.Text)
cdcrsDomainConfigurationName = Lens.lens (domainConfigurationName :: CreateDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationName = a} :: CreateDomainConfigurationResponse)
{-# DEPRECATED cdcrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDomainConfigurationARN :: Lens.Lens' CreateDomainConfigurationResponse (Lude.Maybe Lude.Text)
cdcrsDomainConfigurationARN = Lens.lens (domainConfigurationARN :: CreateDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationARN = a} :: CreateDomainConfigurationResponse)
{-# DEPRECATED cdcrsDomainConfigurationARN "Use generic-lens or generic-optics with 'domainConfigurationARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDomainConfigurationResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDomainConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDomainConfigurationResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

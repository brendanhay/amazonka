{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about a domain configuration.
module Network.AWS.IoT.DescribeDomainConfiguration
  ( -- * Creating a request
    DescribeDomainConfiguration (..),
    mkDescribeDomainConfiguration,

    -- ** Request lenses
    ddcDomainConfigurationName,

    -- * Destructuring the response
    DescribeDomainConfigurationResponse (..),
    mkDescribeDomainConfigurationResponse,

    -- ** Response lenses
    ddcrsDomainConfigurationName,
    ddcrsServerCertificates,
    ddcrsAuthorizerConfig,
    ddcrsLastStatusChangeDate,
    ddcrsDomainConfigurationStatus,
    ddcrsDomainName,
    ddcrsDomainConfigurationARN,
    ddcrsServiceType,
    ddcrsDomainType,
    ddcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDomainConfiguration' smart constructor.
newtype DescribeDomainConfiguration = DescribeDomainConfiguration'
  { -- | The name of the domain configuration.
    domainConfigurationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainConfiguration' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration.
mkDescribeDomainConfiguration ::
  -- | 'domainConfigurationName'
  Lude.Text ->
  DescribeDomainConfiguration
mkDescribeDomainConfiguration pDomainConfigurationName_ =
  DescribeDomainConfiguration'
    { domainConfigurationName =
        pDomainConfigurationName_
    }

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDomainConfigurationName :: Lens.Lens' DescribeDomainConfiguration Lude.Text
ddcDomainConfigurationName = Lens.lens (domainConfigurationName :: DescribeDomainConfiguration -> Lude.Text) (\s a -> s {domainConfigurationName = a} :: DescribeDomainConfiguration)
{-# DEPRECATED ddcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

instance Lude.AWSRequest DescribeDomainConfiguration where
  type
    Rs DescribeDomainConfiguration =
      DescribeDomainConfigurationResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDomainConfigurationResponse'
            Lude.<$> (x Lude..?> "domainConfigurationName")
            Lude.<*> (x Lude..?> "serverCertificates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "authorizerConfig")
            Lude.<*> (x Lude..?> "lastStatusChangeDate")
            Lude.<*> (x Lude..?> "domainConfigurationStatus")
            Lude.<*> (x Lude..?> "domainName")
            Lude.<*> (x Lude..?> "domainConfigurationArn")
            Lude.<*> (x Lude..?> "serviceType")
            Lude.<*> (x Lude..?> "domainType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDomainConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDomainConfiguration where
  toPath DescribeDomainConfiguration' {..} =
    Lude.mconcat
      ["/domainConfigurations/", Lude.toBS domainConfigurationName]

instance Lude.ToQuery DescribeDomainConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDomainConfigurationResponse' smart constructor.
data DescribeDomainConfigurationResponse = DescribeDomainConfigurationResponse'
  { -- | The name of the domain configuration.
    domainConfigurationName :: Lude.Maybe Lude.Text,
    -- | A list containing summary information about the server certificate included in the domain configuration.
    serverCertificates :: Lude.Maybe [ServerCertificateSummary],
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Lude.Maybe AuthorizerConfig,
    -- | The date and time the domain configuration's status was last changed.
    lastStatusChangeDate :: Lude.Maybe Lude.Timestamp,
    -- | A Boolean value that specifies the current state of the domain configuration.
    domainConfigurationStatus :: Lude.Maybe DomainConfigurationStatus,
    -- | The name of the domain.
    domainName :: Lude.Maybe Lude.Text,
    -- | The ARN of the domain configuration.
    domainConfigurationARN :: Lude.Maybe Lude.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Lude.Maybe ServiceType,
    -- | The type of the domain.
    domainType :: Lude.Maybe DomainType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration.
-- * 'serverCertificates' - A list containing summary information about the server certificate included in the domain configuration.
-- * 'authorizerConfig' - An object that specifies the authorization service for a domain.
-- * 'lastStatusChangeDate' - The date and time the domain configuration's status was last changed.
-- * 'domainConfigurationStatus' - A Boolean value that specifies the current state of the domain configuration.
-- * 'domainName' - The name of the domain.
-- * 'domainConfigurationARN' - The ARN of the domain configuration.
-- * 'serviceType' - The type of service delivered by the endpoint.
-- * 'domainType' - The type of the domain.
-- * 'responseStatus' - The response status code.
mkDescribeDomainConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDomainConfigurationResponse
mkDescribeDomainConfigurationResponse pResponseStatus_ =
  DescribeDomainConfigurationResponse'
    { domainConfigurationName =
        Lude.Nothing,
      serverCertificates = Lude.Nothing,
      authorizerConfig = Lude.Nothing,
      lastStatusChangeDate = Lude.Nothing,
      domainConfigurationStatus = Lude.Nothing,
      domainName = Lude.Nothing,
      domainConfigurationARN = Lude.Nothing,
      serviceType = Lude.Nothing,
      domainType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainConfigurationName :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe Lude.Text)
ddcrsDomainConfigurationName = Lens.lens (domainConfigurationName :: DescribeDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationName = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | A list containing summary information about the server certificate included in the domain configuration.
--
-- /Note:/ Consider using 'serverCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsServerCertificates :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe [ServerCertificateSummary])
ddcrsServerCertificates = Lens.lens (serverCertificates :: DescribeDomainConfigurationResponse -> Lude.Maybe [ServerCertificateSummary]) (\s a -> s {serverCertificates = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsServerCertificates "Use generic-lens or generic-optics with 'serverCertificates' instead." #-}

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsAuthorizerConfig :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe AuthorizerConfig)
ddcrsAuthorizerConfig = Lens.lens (authorizerConfig :: DescribeDomainConfigurationResponse -> Lude.Maybe AuthorizerConfig) (\s a -> s {authorizerConfig = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The date and time the domain configuration's status was last changed.
--
-- /Note:/ Consider using 'lastStatusChangeDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsLastStatusChangeDate :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe Lude.Timestamp)
ddcrsLastStatusChangeDate = Lens.lens (lastStatusChangeDate :: DescribeDomainConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStatusChangeDate = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsLastStatusChangeDate "Use generic-lens or generic-optics with 'lastStatusChangeDate' instead." #-}

-- | A Boolean value that specifies the current state of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainConfigurationStatus :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe DomainConfigurationStatus)
ddcrsDomainConfigurationStatus = Lens.lens (domainConfigurationStatus :: DescribeDomainConfigurationResponse -> Lude.Maybe DomainConfigurationStatus) (\s a -> s {domainConfigurationStatus = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsDomainConfigurationStatus "Use generic-lens or generic-optics with 'domainConfigurationStatus' instead." #-}

-- | The name of the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainName :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe Lude.Text)
ddcrsDomainName = Lens.lens (domainName :: DescribeDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainConfigurationARN :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe Lude.Text)
ddcrsDomainConfigurationARN = Lens.lens (domainConfigurationARN :: DescribeDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationARN = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsDomainConfigurationARN "Use generic-lens or generic-optics with 'domainConfigurationARN' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsServiceType :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe ServiceType)
ddcrsServiceType = Lens.lens (serviceType :: DescribeDomainConfigurationResponse -> Lude.Maybe ServiceType) (\s a -> s {serviceType = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The type of the domain.
--
-- /Note:/ Consider using 'domainType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainType :: Lens.Lens' DescribeDomainConfigurationResponse (Lude.Maybe DomainType)
ddcrsDomainType = Lens.lens (domainType :: DescribeDomainConfigurationResponse -> Lude.Maybe DomainType) (\s a -> s {domainType = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsDomainType "Use generic-lens or generic-optics with 'domainType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DescribeDomainConfigurationResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DescribeDomainConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainConfigurationResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

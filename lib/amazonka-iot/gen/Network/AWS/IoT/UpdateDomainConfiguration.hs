{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates values stored in the domain configuration. Domain configurations for default endpoints can't be updated.
module Network.AWS.IoT.UpdateDomainConfiguration
  ( -- * Creating a request
    UpdateDomainConfiguration (..),
    mkUpdateDomainConfiguration,

    -- ** Request lenses
    udcAuthorizerConfig,
    udcDomainConfigurationStatus,
    udcRemoveAuthorizerConfig,
    udcDomainConfigurationName,

    -- * Destructuring the response
    UpdateDomainConfigurationResponse (..),
    mkUpdateDomainConfigurationResponse,

    -- ** Response lenses
    udcrsDomainConfigurationName,
    udcrsDomainConfigurationARN,
    udcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { authorizerConfig ::
      Lude.Maybe AuthorizerConfig,
    domainConfigurationStatus ::
      Lude.Maybe DomainConfigurationStatus,
    removeAuthorizerConfig ::
      Lude.Maybe Lude.Bool,
    domainConfigurationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainConfiguration' with the minimum fields required to make a request.
--
-- * 'authorizerConfig' - An object that specifies the authorization service for a domain.
-- * 'domainConfigurationName' - The name of the domain configuration to be updated.
-- * 'domainConfigurationStatus' - The status to which the domain configuration should be updated.
-- * 'removeAuthorizerConfig' - Removes the authorization configuration from a domain.
mkUpdateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Lude.Text ->
  UpdateDomainConfiguration
mkUpdateDomainConfiguration pDomainConfigurationName_ =
  UpdateDomainConfiguration'
    { authorizerConfig = Lude.Nothing,
      domainConfigurationStatus = Lude.Nothing,
      removeAuthorizerConfig = Lude.Nothing,
      domainConfigurationName = pDomainConfigurationName_
    }

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Lude.Maybe AuthorizerConfig)
udcAuthorizerConfig = Lens.lens (authorizerConfig :: UpdateDomainConfiguration -> Lude.Maybe AuthorizerConfig) (\s a -> s {authorizerConfig = a} :: UpdateDomainConfiguration)
{-# DEPRECATED udcAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The status to which the domain configuration should be updated.
--
-- /Note:/ Consider using 'domainConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainConfigurationStatus :: Lens.Lens' UpdateDomainConfiguration (Lude.Maybe DomainConfigurationStatus)
udcDomainConfigurationStatus = Lens.lens (domainConfigurationStatus :: UpdateDomainConfiguration -> Lude.Maybe DomainConfigurationStatus) (\s a -> s {domainConfigurationStatus = a} :: UpdateDomainConfiguration)
{-# DEPRECATED udcDomainConfigurationStatus "Use generic-lens or generic-optics with 'domainConfigurationStatus' instead." #-}

-- | Removes the authorization configuration from a domain.
--
-- /Note:/ Consider using 'removeAuthorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcRemoveAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Lude.Maybe Lude.Bool)
udcRemoveAuthorizerConfig = Lens.lens (removeAuthorizerConfig :: UpdateDomainConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {removeAuthorizerConfig = a} :: UpdateDomainConfiguration)
{-# DEPRECATED udcRemoveAuthorizerConfig "Use generic-lens or generic-optics with 'removeAuthorizerConfig' instead." #-}

-- | The name of the domain configuration to be updated.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainConfigurationName :: Lens.Lens' UpdateDomainConfiguration Lude.Text
udcDomainConfigurationName = Lens.lens (domainConfigurationName :: UpdateDomainConfiguration -> Lude.Text) (\s a -> s {domainConfigurationName = a} :: UpdateDomainConfiguration)
{-# DEPRECATED udcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

instance Lude.AWSRequest UpdateDomainConfiguration where
  type
    Rs UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            Lude.<$> (x Lude..?> "domainConfigurationName")
            Lude.<*> (x Lude..?> "domainConfigurationArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDomainConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authorizerConfig" Lude..=) Lude.<$> authorizerConfig,
            ("domainConfigurationStatus" Lude..=)
              Lude.<$> domainConfigurationStatus,
            ("removeAuthorizerConfig" Lude..=)
              Lude.<$> removeAuthorizerConfig
          ]
      )

instance Lude.ToPath UpdateDomainConfiguration where
  toPath UpdateDomainConfiguration' {..} =
    Lude.mconcat
      ["/domainConfigurations/", Lude.toBS domainConfigurationName]

instance Lude.ToQuery UpdateDomainConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDomainConfigurationResponse' smart constructor.
data UpdateDomainConfigurationResponse = UpdateDomainConfigurationResponse'
  { domainConfigurationName ::
      Lude.Maybe Lude.Text,
    domainConfigurationARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'domainConfigurationARN' - The ARN of the domain configuration that was updated.
-- * 'domainConfigurationName' - The name of the domain configuration that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateDomainConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDomainConfigurationResponse
mkUpdateDomainConfigurationResponse pResponseStatus_ =
  UpdateDomainConfigurationResponse'
    { domainConfigurationName =
        Lude.Nothing,
      domainConfigurationARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the domain configuration that was updated.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsDomainConfigurationName :: Lens.Lens' UpdateDomainConfigurationResponse (Lude.Maybe Lude.Text)
udcrsDomainConfigurationName = Lens.lens (domainConfigurationName :: UpdateDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationName = a} :: UpdateDomainConfigurationResponse)
{-# DEPRECATED udcrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | The ARN of the domain configuration that was updated.
--
-- /Note:/ Consider using 'domainConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsDomainConfigurationARN :: Lens.Lens' UpdateDomainConfigurationResponse (Lude.Maybe Lude.Text)
udcrsDomainConfigurationARN = Lens.lens (domainConfigurationARN :: UpdateDomainConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationARN = a} :: UpdateDomainConfigurationResponse)
{-# DEPRECATED udcrsDomainConfigurationARN "Use generic-lens or generic-optics with 'domainConfigurationARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsResponseStatus :: Lens.Lens' UpdateDomainConfigurationResponse Lude.Int
udcrsResponseStatus = Lens.lens (responseStatus :: UpdateDomainConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainConfigurationResponse)
{-# DEPRECATED udcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

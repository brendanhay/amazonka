{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration item that is aggregated for your specific resource in a specific source account and region.
module Network.AWS.Config.GetAggregateResourceConfig
  ( -- * Creating a request
    GetAggregateResourceConfig (..),
    mkGetAggregateResourceConfig,

    -- ** Request lenses
    garcConfigurationAggregatorName,
    garcResourceIdentifier,

    -- * Destructuring the response
    GetAggregateResourceConfigResponse (..),
    mkGetAggregateResourceConfigResponse,

    -- ** Response lenses
    garcrsConfigurationItem,
    garcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Lude.Text,
    -- | An object that identifies aggregate resource.
    resourceIdentifier :: AggregateResourceIdentifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAggregateResourceConfig' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'resourceIdentifier' - An object that identifies aggregate resource.
mkGetAggregateResourceConfig ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  AggregateResourceIdentifier ->
  GetAggregateResourceConfig
mkGetAggregateResourceConfig
  pConfigurationAggregatorName_
  pResourceIdentifier_ =
    GetAggregateResourceConfig'
      { configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcConfigurationAggregatorName :: Lens.Lens' GetAggregateResourceConfig Lude.Text
garcConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: GetAggregateResourceConfig -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: GetAggregateResourceConfig)
{-# DEPRECATED garcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | An object that identifies aggregate resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcResourceIdentifier :: Lens.Lens' GetAggregateResourceConfig AggregateResourceIdentifier
garcResourceIdentifier = Lens.lens (resourceIdentifier :: GetAggregateResourceConfig -> AggregateResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: GetAggregateResourceConfig)
{-# DEPRECATED garcResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.AWSRequest GetAggregateResourceConfig where
  type
    Rs GetAggregateResourceConfig =
      GetAggregateResourceConfigResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAggregateResourceConfigResponse'
            Lude.<$> (x Lude..?> "ConfigurationItem")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAggregateResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetAggregateResourceConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAggregateResourceConfig where
  toJSON GetAggregateResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              ),
            Lude.Just ("ResourceIdentifier" Lude..= resourceIdentifier)
          ]
      )

instance Lude.ToPath GetAggregateResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAggregateResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { -- | Returns a @ConfigurationItem@ object.
    configurationItem :: Lude.Maybe ConfigurationItem,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAggregateResourceConfigResponse' with the minimum fields required to make a request.
--
-- * 'configurationItem' - Returns a @ConfigurationItem@ object.
-- * 'responseStatus' - The response status code.
mkGetAggregateResourceConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAggregateResourceConfigResponse
mkGetAggregateResourceConfigResponse pResponseStatus_ =
  GetAggregateResourceConfigResponse'
    { configurationItem =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a @ConfigurationItem@ object.
--
-- /Note:/ Consider using 'configurationItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrsConfigurationItem :: Lens.Lens' GetAggregateResourceConfigResponse (Lude.Maybe ConfigurationItem)
garcrsConfigurationItem = Lens.lens (configurationItem :: GetAggregateResourceConfigResponse -> Lude.Maybe ConfigurationItem) (\s a -> s {configurationItem = a} :: GetAggregateResourceConfigResponse)
{-# DEPRECATED garcrsConfigurationItem "Use generic-lens or generic-optics with 'configurationItem' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrsResponseStatus :: Lens.Lens' GetAggregateResourceConfigResponse Lude.Int
garcrsResponseStatus = Lens.lens (responseStatus :: GetAggregateResourceConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAggregateResourceConfigResponse)
{-# DEPRECATED garcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

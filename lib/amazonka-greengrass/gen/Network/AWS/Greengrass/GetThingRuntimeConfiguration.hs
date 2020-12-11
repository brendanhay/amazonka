{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime configuration of a thing.
module Network.AWS.Greengrass.GetThingRuntimeConfiguration
  ( -- * Creating a request
    GetThingRuntimeConfiguration (..),
    mkGetThingRuntimeConfiguration,

    -- ** Request lenses
    gtrcThingName,

    -- * Destructuring the response
    GetThingRuntimeConfigurationResponse (..),
    mkGetThingRuntimeConfigurationResponse,

    -- ** Response lenses
    gtrcrsRuntimeConfiguration,
    gtrcrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetThingRuntimeConfiguration' smart constructor.
newtype GetThingRuntimeConfiguration = GetThingRuntimeConfiguration'
  { thingName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThingRuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'thingName' - The thing name.
mkGetThingRuntimeConfiguration ::
  -- | 'thingName'
  Lude.Text ->
  GetThingRuntimeConfiguration
mkGetThingRuntimeConfiguration pThingName_ =
  GetThingRuntimeConfiguration' {thingName = pThingName_}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcThingName :: Lens.Lens' GetThingRuntimeConfiguration Lude.Text
gtrcThingName = Lens.lens (thingName :: GetThingRuntimeConfiguration -> Lude.Text) (\s a -> s {thingName = a} :: GetThingRuntimeConfiguration)
{-# DEPRECATED gtrcThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest GetThingRuntimeConfiguration where
  type
    Rs GetThingRuntimeConfiguration =
      GetThingRuntimeConfigurationResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetThingRuntimeConfigurationResponse'
            Lude.<$> (x Lude..?> "RuntimeConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetThingRuntimeConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetThingRuntimeConfiguration where
  toPath GetThingRuntimeConfiguration' {..} =
    Lude.mconcat
      ["/greengrass/things/", Lude.toBS thingName, "/runtimeconfig"]

instance Lude.ToQuery GetThingRuntimeConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetThingRuntimeConfigurationResponse' smart constructor.
data GetThingRuntimeConfigurationResponse = GetThingRuntimeConfigurationResponse'
  { runtimeConfiguration ::
      Lude.Maybe
        RuntimeConfiguration,
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

-- | Creates a value of 'GetThingRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'runtimeConfiguration' - Runtime configuration for a thing.
mkGetThingRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetThingRuntimeConfigurationResponse
mkGetThingRuntimeConfigurationResponse pResponseStatus_ =
  GetThingRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Runtime configuration for a thing.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcrsRuntimeConfiguration :: Lens.Lens' GetThingRuntimeConfigurationResponse (Lude.Maybe RuntimeConfiguration)
gtrcrsRuntimeConfiguration = Lens.lens (runtimeConfiguration :: GetThingRuntimeConfigurationResponse -> Lude.Maybe RuntimeConfiguration) (\s a -> s {runtimeConfiguration = a} :: GetThingRuntimeConfigurationResponse)
{-# DEPRECATED gtrcrsRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcrsResponseStatus :: Lens.Lens' GetThingRuntimeConfigurationResponse Lude.Int
gtrcrsResponseStatus = Lens.lens (responseStatus :: GetThingRuntimeConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetThingRuntimeConfigurationResponse)
{-# DEPRECATED gtrcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

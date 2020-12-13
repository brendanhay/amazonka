{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the runtime configuration of a thing.
module Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
  ( -- * Creating a request
    UpdateThingRuntimeConfiguration (..),
    mkUpdateThingRuntimeConfiguration,

    -- ** Request lenses
    utrcTelemetryConfiguration,
    utrcThingName,

    -- * Destructuring the response
    UpdateThingRuntimeConfigurationResponse (..),
    mkUpdateThingRuntimeConfigurationResponse,

    -- ** Response lenses
    utrcrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateThingRuntimeConfiguration' smart constructor.
data UpdateThingRuntimeConfiguration = UpdateThingRuntimeConfiguration'
  { -- | Configuration for telemetry service.
    telemetryConfiguration :: Lude.Maybe TelemetryConfigurationUpdate,
    -- | The thing name.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingRuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'telemetryConfiguration' - Configuration for telemetry service.
-- * 'thingName' - The thing name.
mkUpdateThingRuntimeConfiguration ::
  -- | 'thingName'
  Lude.Text ->
  UpdateThingRuntimeConfiguration
mkUpdateThingRuntimeConfiguration pThingName_ =
  UpdateThingRuntimeConfiguration'
    { telemetryConfiguration =
        Lude.Nothing,
      thingName = pThingName_
    }

-- | Configuration for telemetry service.
--
-- /Note:/ Consider using 'telemetryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcTelemetryConfiguration :: Lens.Lens' UpdateThingRuntimeConfiguration (Lude.Maybe TelemetryConfigurationUpdate)
utrcTelemetryConfiguration = Lens.lens (telemetryConfiguration :: UpdateThingRuntimeConfiguration -> Lude.Maybe TelemetryConfigurationUpdate) (\s a -> s {telemetryConfiguration = a} :: UpdateThingRuntimeConfiguration)
{-# DEPRECATED utrcTelemetryConfiguration "Use generic-lens or generic-optics with 'telemetryConfiguration' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcThingName :: Lens.Lens' UpdateThingRuntimeConfiguration Lude.Text
utrcThingName = Lens.lens (thingName :: UpdateThingRuntimeConfiguration -> Lude.Text) (\s a -> s {thingName = a} :: UpdateThingRuntimeConfiguration)
{-# DEPRECATED utrcThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest UpdateThingRuntimeConfiguration where
  type
    Rs UpdateThingRuntimeConfiguration =
      UpdateThingRuntimeConfigurationResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateThingRuntimeConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateThingRuntimeConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateThingRuntimeConfiguration where
  toJSON UpdateThingRuntimeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TelemetryConfiguration" Lude..=)
              Lude.<$> telemetryConfiguration
          ]
      )

instance Lude.ToPath UpdateThingRuntimeConfiguration where
  toPath UpdateThingRuntimeConfiguration' {..} =
    Lude.mconcat
      ["/greengrass/things/", Lude.toBS thingName, "/runtimeconfig"]

instance Lude.ToQuery UpdateThingRuntimeConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateThingRuntimeConfigurationResponse' smart constructor.
newtype UpdateThingRuntimeConfigurationResponse = UpdateThingRuntimeConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateThingRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThingRuntimeConfigurationResponse
mkUpdateThingRuntimeConfigurationResponse pResponseStatus_ =
  UpdateThingRuntimeConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcrsResponseStatus :: Lens.Lens' UpdateThingRuntimeConfigurationResponse Lude.Int
utrcrsResponseStatus = Lens.lens (responseStatus :: UpdateThingRuntimeConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThingRuntimeConfigurationResponse)
{-# DEPRECATED utrcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

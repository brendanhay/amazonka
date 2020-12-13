{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the indexing configuration.
module Network.AWS.IoT.GetIndexingConfiguration
  ( -- * Creating a request
    GetIndexingConfiguration (..),
    mkGetIndexingConfiguration,

    -- * Destructuring the response
    GetIndexingConfigurationResponse (..),
    mkGetIndexingConfigurationResponse,

    -- ** Response lenses
    gicrsThingGroupIndexingConfiguration,
    gicrsThingIndexingConfiguration,
    gicrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIndexingConfiguration' smart constructor.
data GetIndexingConfiguration = GetIndexingConfiguration'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIndexingConfiguration' with the minimum fields required to make a request.
mkGetIndexingConfiguration ::
  GetIndexingConfiguration
mkGetIndexingConfiguration = GetIndexingConfiguration'

instance Lude.AWSRequest GetIndexingConfiguration where
  type Rs GetIndexingConfiguration = GetIndexingConfigurationResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIndexingConfigurationResponse'
            Lude.<$> (x Lude..?> "thingGroupIndexingConfiguration")
            Lude.<*> (x Lude..?> "thingIndexingConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIndexingConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIndexingConfiguration where
  toPath = Lude.const "/indexing/config"

instance Lude.ToQuery GetIndexingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetIndexingConfigurationResponse' smart constructor.
data GetIndexingConfigurationResponse = GetIndexingConfigurationResponse'
  { -- | The index configuration.
    thingGroupIndexingConfiguration :: Lude.Maybe ThingGroupIndexingConfiguration,
    -- | Thing indexing configuration.
    thingIndexingConfiguration :: Lude.Maybe ThingIndexingConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIndexingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'thingGroupIndexingConfiguration' - The index configuration.
-- * 'thingIndexingConfiguration' - Thing indexing configuration.
-- * 'responseStatus' - The response status code.
mkGetIndexingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIndexingConfigurationResponse
mkGetIndexingConfigurationResponse pResponseStatus_ =
  GetIndexingConfigurationResponse'
    { thingGroupIndexingConfiguration =
        Lude.Nothing,
      thingIndexingConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The index configuration.
--
-- /Note:/ Consider using 'thingGroupIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsThingGroupIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Lude.Maybe ThingGroupIndexingConfiguration)
gicrsThingGroupIndexingConfiguration = Lens.lens (thingGroupIndexingConfiguration :: GetIndexingConfigurationResponse -> Lude.Maybe ThingGroupIndexingConfiguration) (\s a -> s {thingGroupIndexingConfiguration = a} :: GetIndexingConfigurationResponse)
{-# DEPRECATED gicrsThingGroupIndexingConfiguration "Use generic-lens or generic-optics with 'thingGroupIndexingConfiguration' instead." #-}

-- | Thing indexing configuration.
--
-- /Note:/ Consider using 'thingIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsThingIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Lude.Maybe ThingIndexingConfiguration)
gicrsThingIndexingConfiguration = Lens.lens (thingIndexingConfiguration :: GetIndexingConfigurationResponse -> Lude.Maybe ThingIndexingConfiguration) (\s a -> s {thingIndexingConfiguration = a} :: GetIndexingConfigurationResponse)
{-# DEPRECATED gicrsThingIndexingConfiguration "Use generic-lens or generic-optics with 'thingIndexingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsResponseStatus :: Lens.Lens' GetIndexingConfigurationResponse Lude.Int
gicrsResponseStatus = Lens.lens (responseStatus :: GetIndexingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIndexingConfigurationResponse)
{-# DEPRECATED gicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the search configuration.
module Network.AWS.IoT.UpdateIndexingConfiguration
  ( -- * Creating a request
    UpdateIndexingConfiguration (..),
    mkUpdateIndexingConfiguration,

    -- ** Request lenses
    uicThingGroupIndexingConfiguration,
    uicThingIndexingConfiguration,

    -- * Destructuring the response
    UpdateIndexingConfigurationResponse (..),
    mkUpdateIndexingConfigurationResponse,

    -- ** Response lenses
    uicrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateIndexingConfiguration' smart constructor.
data UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { thingGroupIndexingConfiguration ::
      Lude.Maybe
        ThingGroupIndexingConfiguration,
    thingIndexingConfiguration ::
      Lude.Maybe
        ThingIndexingConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIndexingConfiguration' with the minimum fields required to make a request.
--
-- * 'thingGroupIndexingConfiguration' - Thing group indexing configuration.
-- * 'thingIndexingConfiguration' - Thing indexing configuration.
mkUpdateIndexingConfiguration ::
  UpdateIndexingConfiguration
mkUpdateIndexingConfiguration =
  UpdateIndexingConfiguration'
    { thingGroupIndexingConfiguration =
        Lude.Nothing,
      thingIndexingConfiguration = Lude.Nothing
    }

-- | Thing group indexing configuration.
--
-- /Note:/ Consider using 'thingGroupIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicThingGroupIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Lude.Maybe ThingGroupIndexingConfiguration)
uicThingGroupIndexingConfiguration = Lens.lens (thingGroupIndexingConfiguration :: UpdateIndexingConfiguration -> Lude.Maybe ThingGroupIndexingConfiguration) (\s a -> s {thingGroupIndexingConfiguration = a} :: UpdateIndexingConfiguration)
{-# DEPRECATED uicThingGroupIndexingConfiguration "Use generic-lens or generic-optics with 'thingGroupIndexingConfiguration' instead." #-}

-- | Thing indexing configuration.
--
-- /Note:/ Consider using 'thingIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicThingIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Lude.Maybe ThingIndexingConfiguration)
uicThingIndexingConfiguration = Lens.lens (thingIndexingConfiguration :: UpdateIndexingConfiguration -> Lude.Maybe ThingIndexingConfiguration) (\s a -> s {thingIndexingConfiguration = a} :: UpdateIndexingConfiguration)
{-# DEPRECATED uicThingIndexingConfiguration "Use generic-lens or generic-optics with 'thingIndexingConfiguration' instead." #-}

instance Lude.AWSRequest UpdateIndexingConfiguration where
  type
    Rs UpdateIndexingConfiguration =
      UpdateIndexingConfigurationResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateIndexingConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateIndexingConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateIndexingConfiguration where
  toJSON UpdateIndexingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingGroupIndexingConfiguration" Lude..=)
              Lude.<$> thingGroupIndexingConfiguration,
            ("thingIndexingConfiguration" Lude..=)
              Lude.<$> thingIndexingConfiguration
          ]
      )

instance Lude.ToPath UpdateIndexingConfiguration where
  toPath = Lude.const "/indexing/config"

instance Lude.ToQuery UpdateIndexingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateIndexingConfigurationResponse' smart constructor.
newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIndexingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateIndexingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateIndexingConfigurationResponse
mkUpdateIndexingConfigurationResponse pResponseStatus_ =
  UpdateIndexingConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicrsResponseStatus :: Lens.Lens' UpdateIndexingConfigurationResponse Lude.Int
uicrsResponseStatus = Lens.lens (responseStatus :: UpdateIndexingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateIndexingConfigurationResponse)
{-# DEPRECATED uicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

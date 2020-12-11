{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateEventConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event configurations.
module Network.AWS.IoT.UpdateEventConfigurations
  ( -- * Creating a request
    UpdateEventConfigurations (..),
    mkUpdateEventConfigurations,

    -- ** Request lenses
    uecEventConfigurations,

    -- * Destructuring the response
    UpdateEventConfigurationsResponse (..),
    mkUpdateEventConfigurationsResponse,

    -- ** Response lenses
    uecrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEventConfigurations' smart constructor.
newtype UpdateEventConfigurations = UpdateEventConfigurations'
  { eventConfigurations ::
      Lude.Maybe
        ( Lude.HashMap
            EventType
            (Configuration)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEventConfigurations' with the minimum fields required to make a request.
--
-- * 'eventConfigurations' - The new event configuration values.
mkUpdateEventConfigurations ::
  UpdateEventConfigurations
mkUpdateEventConfigurations =
  UpdateEventConfigurations' {eventConfigurations = Lude.Nothing}

-- | The new event configuration values.
--
-- /Note:/ Consider using 'eventConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecEventConfigurations :: Lens.Lens' UpdateEventConfigurations (Lude.Maybe (Lude.HashMap EventType (Configuration)))
uecEventConfigurations = Lens.lens (eventConfigurations :: UpdateEventConfigurations -> Lude.Maybe (Lude.HashMap EventType (Configuration))) (\s a -> s {eventConfigurations = a} :: UpdateEventConfigurations)
{-# DEPRECATED uecEventConfigurations "Use generic-lens or generic-optics with 'eventConfigurations' instead." #-}

instance Lude.AWSRequest UpdateEventConfigurations where
  type
    Rs UpdateEventConfigurations =
      UpdateEventConfigurationsResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateEventConfigurationsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEventConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateEventConfigurations where
  toJSON UpdateEventConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [("eventConfigurations" Lude..=) Lude.<$> eventConfigurations]
      )

instance Lude.ToPath UpdateEventConfigurations where
  toPath = Lude.const "/event-configurations"

instance Lude.ToQuery UpdateEventConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEventConfigurationsResponse' smart constructor.
newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse'
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

-- | Creates a value of 'UpdateEventConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateEventConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEventConfigurationsResponse
mkUpdateEventConfigurationsResponse pResponseStatus_ =
  UpdateEventConfigurationsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrsResponseStatus :: Lens.Lens' UpdateEventConfigurationsResponse Lude.Int
uecrsResponseStatus = Lens.lens (responseStatus :: UpdateEventConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEventConfigurationsResponse)
{-# DEPRECATED uecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event destination of a configuration set. Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity> in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateConfigurationSetEventDestination
  ( -- * Creating a request
    UpdateConfigurationSetEventDestination (..),
    mkUpdateConfigurationSetEventDestination,

    -- ** Request lenses
    ucsedConfigurationSetName,
    ucsedEventDestination,

    -- * Destructuring the response
    UpdateConfigurationSetEventDestinationResponse (..),
    mkUpdateConfigurationSetEventDestinationResponse,

    -- ** Response lenses
    ucsedrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to update the event destination of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkUpdateConfigurationSetEventDestination' smart constructor.
data UpdateConfigurationSetEventDestination = UpdateConfigurationSetEventDestination'
  { -- | The name of the configuration set that contains the event destination that you want to update.
    configurationSetName :: Lude.Text,
    -- | The event destination object that you want to apply to the specified configuration set.
    eventDestination :: EventDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set that contains the event destination that you want to update.
-- * 'eventDestination' - The event destination object that you want to apply to the specified configuration set.
mkUpdateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Lude.Text ->
  -- | 'eventDestination'
  EventDestination ->
  UpdateConfigurationSetEventDestination
mkUpdateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestination_ =
    UpdateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestination = pEventDestination_
      }

-- | The name of the configuration set that contains the event destination that you want to update.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsedConfigurationSetName :: Lens.Lens' UpdateConfigurationSetEventDestination Lude.Text
ucsedConfigurationSetName = Lens.lens (configurationSetName :: UpdateConfigurationSetEventDestination -> Lude.Text) (\s a -> s {configurationSetName = a} :: UpdateConfigurationSetEventDestination)
{-# DEPRECATED ucsedConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | The event destination object that you want to apply to the specified configuration set.
--
-- /Note:/ Consider using 'eventDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsedEventDestination :: Lens.Lens' UpdateConfigurationSetEventDestination EventDestination
ucsedEventDestination = Lens.lens (eventDestination :: UpdateConfigurationSetEventDestination -> EventDestination) (\s a -> s {eventDestination = a} :: UpdateConfigurationSetEventDestination)
{-# DEPRECATED ucsedEventDestination "Use generic-lens or generic-optics with 'eventDestination' instead." #-}

instance Lude.AWSRequest UpdateConfigurationSetEventDestination where
  type
    Rs UpdateConfigurationSetEventDestination =
      UpdateConfigurationSetEventDestinationResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "UpdateConfigurationSetEventDestinationResult"
      ( \s h x ->
          UpdateConfigurationSetEventDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConfigurationSetEventDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateConfigurationSetEventDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConfigurationSetEventDestination where
  toQuery UpdateConfigurationSetEventDestination' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateConfigurationSetEventDestination" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "EventDestination" Lude.=: eventDestination
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkUpdateConfigurationSetEventDestinationResponse' smart constructor.
newtype UpdateConfigurationSetEventDestinationResponse = UpdateConfigurationSetEventDestinationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConfigurationSetEventDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConfigurationSetEventDestinationResponse
mkUpdateConfigurationSetEventDestinationResponse pResponseStatus_ =
  UpdateConfigurationSetEventDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsedrsResponseStatus :: Lens.Lens' UpdateConfigurationSetEventDestinationResponse Lude.Int
ucsedrsResponseStatus = Lens.lens (responseStatus :: UpdateConfigurationSetEventDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConfigurationSetEventDestinationResponse)
{-# DEPRECATED ucsedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

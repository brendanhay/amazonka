{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set event destination.
--
-- An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateConfigurationSetEventDestination
  ( -- * Creating a request
    CreateConfigurationSetEventDestination (..),
    mkCreateConfigurationSetEventDestination,

    -- ** Request lenses
    ccsedConfigurationSetName,
    ccsedEventDestination,

    -- * Destructuring the response
    CreateConfigurationSetEventDestinationResponse (..),
    mkCreateConfigurationSetEventDestinationResponse,

    -- ** Response lenses
    ccsedrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { configurationSetName ::
      Lude.Text,
    eventDestination ::
      EventDestination
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set that the event destination should be associated with.
-- * 'eventDestination' - An object that describes the AWS service that email sending event information will be published to.
mkCreateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Lude.Text ->
  -- | 'eventDestination'
  EventDestination ->
  CreateConfigurationSetEventDestination
mkCreateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestination_ =
    CreateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestination = pEventDestination_
      }

-- | The name of the configuration set that the event destination should be associated with.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedConfigurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Lude.Text
ccsedConfigurationSetName = Lens.lens (configurationSetName :: CreateConfigurationSetEventDestination -> Lude.Text) (\s a -> s {configurationSetName = a} :: CreateConfigurationSetEventDestination)
{-# DEPRECATED ccsedConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | An object that describes the AWS service that email sending event information will be published to.
--
-- /Note:/ Consider using 'eventDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedEventDestination :: Lens.Lens' CreateConfigurationSetEventDestination EventDestination
ccsedEventDestination = Lens.lens (eventDestination :: CreateConfigurationSetEventDestination -> EventDestination) (\s a -> s {eventDestination = a} :: CreateConfigurationSetEventDestination)
{-# DEPRECATED ccsedEventDestination "Use generic-lens or generic-optics with 'eventDestination' instead." #-}

instance Lude.AWSRequest CreateConfigurationSetEventDestination where
  type
    Rs CreateConfigurationSetEventDestination =
      CreateConfigurationSetEventDestinationResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateConfigurationSetEventDestinationResult"
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConfigurationSetEventDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateConfigurationSetEventDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConfigurationSetEventDestination where
  toQuery CreateConfigurationSetEventDestination' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateConfigurationSetEventDestination" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "EventDestination" Lude.=: eventDestination
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetEventDestinationResponse' smart constructor.
newtype CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
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
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateConfigurationSetEventDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConfigurationSetEventDestinationResponse
mkCreateConfigurationSetEventDestinationResponse pResponseStatus_ =
  CreateConfigurationSetEventDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsedrsResponseStatus :: Lens.Lens' CreateConfigurationSetEventDestinationResponse Lude.Int
ccsedrsResponseStatus = Lens.lens (responseStatus :: CreateConfigurationSetEventDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConfigurationSetEventDestinationResponse)
{-# DEPRECATED ccsedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

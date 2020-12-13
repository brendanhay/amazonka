{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteConfigurationSetEventDestination
  ( -- * Creating a request
    DeleteConfigurationSetEventDestination (..),
    mkDeleteConfigurationSetEventDestination,

    -- ** Request lenses
    dcsedConfigurationSetName,
    dcsedEventDestinationName,

    -- * Destructuring the response
    DeleteConfigurationSetEventDestinationResponse (..),
    mkDeleteConfigurationSetEventDestinationResponse,

    -- ** Response lenses
    dcsedrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { -- | The name of the configuration set from which to delete the event destination.
    configurationSetName :: Lude.Text,
    -- | The name of the event destination to delete.
    eventDestinationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set from which to delete the event destination.
-- * 'eventDestinationName' - The name of the event destination to delete.
mkDeleteConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Lude.Text ->
  -- | 'eventDestinationName'
  Lude.Text ->
  DeleteConfigurationSetEventDestination
mkDeleteConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestinationName_ =
    DeleteConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestinationName = pEventDestinationName_
      }

-- | The name of the configuration set from which to delete the event destination.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedConfigurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Lude.Text
dcsedConfigurationSetName = Lens.lens (configurationSetName :: DeleteConfigurationSetEventDestination -> Lude.Text) (\s a -> s {configurationSetName = a} :: DeleteConfigurationSetEventDestination)
{-# DEPRECATED dcsedConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | The name of the event destination to delete.
--
-- /Note:/ Consider using 'eventDestinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedEventDestinationName :: Lens.Lens' DeleteConfigurationSetEventDestination Lude.Text
dcsedEventDestinationName = Lens.lens (eventDestinationName :: DeleteConfigurationSetEventDestination -> Lude.Text) (\s a -> s {eventDestinationName = a} :: DeleteConfigurationSetEventDestination)
{-# DEPRECATED dcsedEventDestinationName "Use generic-lens or generic-optics with 'eventDestinationName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationSetEventDestination where
  type
    Rs DeleteConfigurationSetEventDestination =
      DeleteConfigurationSetEventDestinationResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteConfigurationSetEventDestinationResult"
      ( \s h x ->
          DeleteConfigurationSetEventDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConfigurationSetEventDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteConfigurationSetEventDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationSetEventDestination where
  toQuery DeleteConfigurationSetEventDestination' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteConfigurationSetEventDestination" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "EventDestinationName" Lude.=: eventDestinationName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetEventDestinationResponse' smart constructor.
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConfigurationSetEventDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConfigurationSetEventDestinationResponse
mkDeleteConfigurationSetEventDestinationResponse pResponseStatus_ =
  DeleteConfigurationSetEventDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsedrsResponseStatus :: Lens.Lens' DeleteConfigurationSetEventDestinationResponse Lude.Int
dcsedrsResponseStatus = Lens.lens (responseStatus :: DeleteConfigurationSetEventDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConfigurationSetEventDestinationResponse)
{-# DEPRECATED dcsedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified configuration set. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeConfigurationSet
  ( -- * Creating a request
    DescribeConfigurationSet (..),
    mkDescribeConfigurationSet,

    -- ** Request lenses
    dConfigurationSetAttributeNames,
    dConfigurationSetName,

    -- * Destructuring the response
    DescribeConfigurationSetResponse (..),
    mkDescribeConfigurationSetResponse,

    -- ** Response lenses
    dcsrsDeliveryOptions,
    dcsrsTrackingOptions,
    dcsrsConfigurationSet,
    dcsrsReputationOptions,
    dcsrsEventDestinations,
    dcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeConfigurationSet' smart constructor.
data DescribeConfigurationSet = DescribeConfigurationSet'
  { -- | A list of configuration set attributes to return.
    configurationSetAttributeNames :: Lude.Maybe [ConfigurationSetAttribute],
    -- | The name of the configuration set to describe.
    configurationSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationSet' with the minimum fields required to make a request.
--
-- * 'configurationSetAttributeNames' - A list of configuration set attributes to return.
-- * 'configurationSetName' - The name of the configuration set to describe.
mkDescribeConfigurationSet ::
  -- | 'configurationSetName'
  Lude.Text ->
  DescribeConfigurationSet
mkDescribeConfigurationSet pConfigurationSetName_ =
  DescribeConfigurationSet'
    { configurationSetAttributeNames =
        Lude.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | A list of configuration set attributes to return.
--
-- /Note:/ Consider using 'configurationSetAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigurationSetAttributeNames :: Lens.Lens' DescribeConfigurationSet (Lude.Maybe [ConfigurationSetAttribute])
dConfigurationSetAttributeNames = Lens.lens (configurationSetAttributeNames :: DescribeConfigurationSet -> Lude.Maybe [ConfigurationSetAttribute]) (\s a -> s {configurationSetAttributeNames = a} :: DescribeConfigurationSet)
{-# DEPRECATED dConfigurationSetAttributeNames "Use generic-lens or generic-optics with 'configurationSetAttributeNames' instead." #-}

-- | The name of the configuration set to describe.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigurationSetName :: Lens.Lens' DescribeConfigurationSet Lude.Text
dConfigurationSetName = Lens.lens (configurationSetName :: DescribeConfigurationSet -> Lude.Text) (\s a -> s {configurationSetName = a} :: DescribeConfigurationSet)
{-# DEPRECATED dConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Lude.AWSRequest DescribeConfigurationSet where
  type Rs DescribeConfigurationSet = DescribeConfigurationSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DescribeConfigurationSetResult"
      ( \s h x ->
          DescribeConfigurationSetResponse'
            Lude.<$> (x Lude..@? "DeliveryOptions")
            Lude.<*> (x Lude..@? "TrackingOptions")
            Lude.<*> (x Lude..@? "ConfigurationSet")
            Lude.<*> (x Lude..@? "ReputationOptions")
            Lude.<*> ( x Lude..@? "EventDestinations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeConfigurationSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationSet where
  toQuery DescribeConfigurationSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeConfigurationSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetAttributeNames"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "member"
                Lude.<$> configurationSetAttributeNames
            ),
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeConfigurationSetResponse' smart constructor.
data DescribeConfigurationSetResponse = DescribeConfigurationSetResponse'
  { deliveryOptions :: Lude.Maybe DeliveryOptions,
    -- | The name of the custom open and click tracking domain associated with the configuration set.
    trackingOptions :: Lude.Maybe TrackingOptions,
    -- | The configuration set object associated with the specified configuration set.
    configurationSet :: Lude.Maybe ConfigurationSet,
    -- | An object that represents the reputation settings for the configuration set.
    reputationOptions :: Lude.Maybe ReputationOptions,
    -- | A list of event destinations associated with the configuration set.
    eventDestinations :: Lude.Maybe [EventDestination],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationSetResponse' with the minimum fields required to make a request.
--
-- * 'deliveryOptions' -
-- * 'trackingOptions' - The name of the custom open and click tracking domain associated with the configuration set.
-- * 'configurationSet' - The configuration set object associated with the specified configuration set.
-- * 'reputationOptions' - An object that represents the reputation settings for the configuration set.
-- * 'eventDestinations' - A list of event destinations associated with the configuration set.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationSetResponse
mkDescribeConfigurationSetResponse pResponseStatus_ =
  DescribeConfigurationSetResponse'
    { deliveryOptions = Lude.Nothing,
      trackingOptions = Lude.Nothing,
      configurationSet = Lude.Nothing,
      reputationOptions = Lude.Nothing,
      eventDestinations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'deliveryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsDeliveryOptions :: Lens.Lens' DescribeConfigurationSetResponse (Lude.Maybe DeliveryOptions)
dcsrsDeliveryOptions = Lens.lens (deliveryOptions :: DescribeConfigurationSetResponse -> Lude.Maybe DeliveryOptions) (\s a -> s {deliveryOptions = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsDeliveryOptions "Use generic-lens or generic-optics with 'deliveryOptions' instead." #-}

-- | The name of the custom open and click tracking domain associated with the configuration set.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsTrackingOptions :: Lens.Lens' DescribeConfigurationSetResponse (Lude.Maybe TrackingOptions)
dcsrsTrackingOptions = Lens.lens (trackingOptions :: DescribeConfigurationSetResponse -> Lude.Maybe TrackingOptions) (\s a -> s {trackingOptions = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsTrackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead." #-}

-- | The configuration set object associated with the specified configuration set.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsConfigurationSet :: Lens.Lens' DescribeConfigurationSetResponse (Lude.Maybe ConfigurationSet)
dcsrsConfigurationSet = Lens.lens (configurationSet :: DescribeConfigurationSetResponse -> Lude.Maybe ConfigurationSet) (\s a -> s {configurationSet = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsConfigurationSet "Use generic-lens or generic-optics with 'configurationSet' instead." #-}

-- | An object that represents the reputation settings for the configuration set.
--
-- /Note:/ Consider using 'reputationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsReputationOptions :: Lens.Lens' DescribeConfigurationSetResponse (Lude.Maybe ReputationOptions)
dcsrsReputationOptions = Lens.lens (reputationOptions :: DescribeConfigurationSetResponse -> Lude.Maybe ReputationOptions) (\s a -> s {reputationOptions = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsReputationOptions "Use generic-lens or generic-optics with 'reputationOptions' instead." #-}

-- | A list of event destinations associated with the configuration set.
--
-- /Note:/ Consider using 'eventDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsEventDestinations :: Lens.Lens' DescribeConfigurationSetResponse (Lude.Maybe [EventDestination])
dcsrsEventDestinations = Lens.lens (eventDestinations :: DescribeConfigurationSetResponse -> Lude.Maybe [EventDestination]) (\s a -> s {eventDestinations = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsEventDestinations "Use generic-lens or generic-optics with 'eventDestinations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DescribeConfigurationSetResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationSetResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

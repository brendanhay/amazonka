{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.PutEventSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an event selector for your trail. Use event selectors to further specify the management and data event settings for your trail. By default, trails created without specific event selectors will be configured to log all read and write management events, and no data events.
--
-- When an event occurs in your account, CloudTrail evaluates the event selectors in all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
-- Example
--
--     * You create an event selector for a trail and specify that you want write-only events.
--
--
--     * The EC2 @GetConsoleOutput@ and @RunInstances@ API operations occur in your account.
--
--
--     * CloudTrail evaluates whether the events match your event selectors.
--
--
--     * The @RunInstances@ is a write-only event and it matches your event selector. The trail logs the event.
--
--
--     * The @GetConsoleOutput@ is a read-only event but it doesn't match your event selector. The trail doesn't log the event.
--
--
-- The @PutEventSelectors@ operation must be called from the region in which the trail was created; otherwise, an @InvalidHomeRegionException@ is thrown.
-- You can configure up to five event selectors for each trail. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.PutEventSelectors
  ( -- * Creating a request
    PutEventSelectors (..),
    mkPutEventSelectors,

    -- ** Request lenses
    pesEventSelectors,
    pesAdvancedEventSelectors,
    pesTrailName,

    -- * Destructuring the response
    PutEventSelectorsResponse (..),
    mkPutEventSelectorsResponse,

    -- ** Response lenses
    pesrsTrailARN,
    pesrsEventSelectors,
    pesrsAdvancedEventSelectors,
    pesrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutEventSelectors' smart constructor.
data PutEventSelectors = PutEventSelectors'
  { -- | Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
    eventSelectors :: Lude.Maybe [EventSelector],
    advancedEventSelectors :: Lude.Maybe [AdvancedEventSelector],
    -- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:
    --
    --
    --     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
    --
    --
    --     * Start with a letter or number, and end with a letter or number
    --
    --
    --     * Be between 3 and 128 characters
    --
    --
    --     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
    --
    --
    --     * Not be in IP address format (for example, 192.168.5.4)
    --
    --
    -- If you specify a trail ARN, it must be in the format:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventSelectors' with the minimum fields required to make a request.
--
-- * 'eventSelectors' - Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
-- * 'advancedEventSelectors' -
-- * 'trailName' - Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkPutEventSelectors ::
  -- | 'trailName'
  Lude.Text ->
  PutEventSelectors
mkPutEventSelectors pTrailName_ =
  PutEventSelectors'
    { eventSelectors = Lude.Nothing,
      advancedEventSelectors = Lude.Nothing,
      trailName = pTrailName_
    }

-- | Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesEventSelectors :: Lens.Lens' PutEventSelectors (Lude.Maybe [EventSelector])
pesEventSelectors = Lens.lens (eventSelectors :: PutEventSelectors -> Lude.Maybe [EventSelector]) (\s a -> s {eventSelectors = a} :: PutEventSelectors)
{-# DEPRECATED pesEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesAdvancedEventSelectors :: Lens.Lens' PutEventSelectors (Lude.Maybe [AdvancedEventSelector])
pesAdvancedEventSelectors = Lens.lens (advancedEventSelectors :: PutEventSelectors -> Lude.Maybe [AdvancedEventSelector]) (\s a -> s {advancedEventSelectors = a} :: PutEventSelectors)
{-# DEPRECATED pesAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesTrailName :: Lens.Lens' PutEventSelectors Lude.Text
pesTrailName = Lens.lens (trailName :: PutEventSelectors -> Lude.Text) (\s a -> s {trailName = a} :: PutEventSelectors)
{-# DEPRECATED pesTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

instance Lude.AWSRequest PutEventSelectors where
  type Rs PutEventSelectors = PutEventSelectorsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutEventSelectorsResponse'
            Lude.<$> (x Lude..?> "TrailARN")
            Lude.<*> (x Lude..?> "EventSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AdvancedEventSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutEventSelectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutEventSelectors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutEventSelectors where
  toJSON PutEventSelectors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventSelectors" Lude..=) Lude.<$> eventSelectors,
            ("AdvancedEventSelectors" Lude..=) Lude.<$> advancedEventSelectors,
            Lude.Just ("TrailName" Lude..= trailName)
          ]
      )

instance Lude.ToPath PutEventSelectors where
  toPath = Lude.const "/"

instance Lude.ToQuery PutEventSelectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutEventSelectorsResponse' smart constructor.
data PutEventSelectorsResponse = PutEventSelectorsResponse'
  { -- | Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Lude.Maybe Lude.Text,
    -- | Specifies the event selectors configured for your trail.
    eventSelectors :: Lude.Maybe [EventSelector],
    advancedEventSelectors :: Lude.Maybe [AdvancedEventSelector],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventSelectorsResponse' with the minimum fields required to make a request.
--
-- * 'trailARN' - Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- * 'eventSelectors' - Specifies the event selectors configured for your trail.
-- * 'advancedEventSelectors' -
-- * 'responseStatus' - The response status code.
mkPutEventSelectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutEventSelectorsResponse
mkPutEventSelectorsResponse pResponseStatus_ =
  PutEventSelectorsResponse'
    { trailARN = Lude.Nothing,
      eventSelectors = Lude.Nothing,
      advancedEventSelectors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsTrailARN :: Lens.Lens' PutEventSelectorsResponse (Lude.Maybe Lude.Text)
pesrsTrailARN = Lens.lens (trailARN :: PutEventSelectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: PutEventSelectorsResponse)
{-# DEPRECATED pesrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | Specifies the event selectors configured for your trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Lude.Maybe [EventSelector])
pesrsEventSelectors = Lens.lens (eventSelectors :: PutEventSelectorsResponse -> Lude.Maybe [EventSelector]) (\s a -> s {eventSelectors = a} :: PutEventSelectorsResponse)
{-# DEPRECATED pesrsEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsAdvancedEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Lude.Maybe [AdvancedEventSelector])
pesrsAdvancedEventSelectors = Lens.lens (advancedEventSelectors :: PutEventSelectorsResponse -> Lude.Maybe [AdvancedEventSelector]) (\s a -> s {advancedEventSelectors = a} :: PutEventSelectorsResponse)
{-# DEPRECATED pesrsAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrsResponseStatus :: Lens.Lens' PutEventSelectorsResponse Lude.Int
pesrsResponseStatus = Lens.lens (responseStatus :: PutEventSelectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutEventSelectorsResponse)
{-# DEPRECATED pesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

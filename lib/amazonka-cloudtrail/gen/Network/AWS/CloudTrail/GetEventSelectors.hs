{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetEventSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the event selectors that you configured for your trail. The information returned for your event selectors includes the following:
--
--
--     * If your event selector includes read-only events, write-only events, or all events. This applies to both management events and data events.
--
--
--     * If your event selector includes management events.
--
--
--     * If your event selector includes data events, the Amazon S3 objects or AWS Lambda functions that you are logging for data events.
--
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.GetEventSelectors
  ( -- * Creating a request
    GetEventSelectors (..),
    mkGetEventSelectors,

    -- ** Request lenses
    gesTrailName,

    -- * Destructuring the response
    GetEventSelectorsResponse (..),
    mkGetEventSelectorsResponse,

    -- ** Response lenses
    gesrsTrailARN,
    gesrsEventSelectors,
    gesrsAdvancedEventSelectors,
    gesrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEventSelectors' smart constructor.
newtype GetEventSelectors = GetEventSelectors'
  { trailName ::
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

-- | Creates a value of 'GetEventSelectors' with the minimum fields required to make a request.
--
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
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are not valid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkGetEventSelectors ::
  -- | 'trailName'
  Lude.Text ->
  GetEventSelectors
mkGetEventSelectors pTrailName_ =
  GetEventSelectors' {trailName = pTrailName_}

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
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are not valid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesTrailName :: Lens.Lens' GetEventSelectors Lude.Text
gesTrailName = Lens.lens (trailName :: GetEventSelectors -> Lude.Text) (\s a -> s {trailName = a} :: GetEventSelectors)
{-# DEPRECATED gesTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

instance Lude.AWSRequest GetEventSelectors where
  type Rs GetEventSelectors = GetEventSelectorsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEventSelectorsResponse'
            Lude.<$> (x Lude..?> "TrailARN")
            Lude.<*> (x Lude..?> "EventSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AdvancedEventSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEventSelectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventSelectors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetEventSelectors where
  toJSON GetEventSelectors' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TrailName" Lude..= trailName)])

instance Lude.ToPath GetEventSelectors where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEventSelectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetEventSelectorsResponse' smart constructor.
data GetEventSelectorsResponse = GetEventSelectorsResponse'
  { trailARN ::
      Lude.Maybe Lude.Text,
    eventSelectors ::
      Lude.Maybe [EventSelector],
    advancedEventSelectors ::
      Lude.Maybe [AdvancedEventSelector],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEventSelectorsResponse' with the minimum fields required to make a request.
--
-- * 'advancedEventSelectors' - Undocumented field.
-- * 'eventSelectors' - The event selectors that are configured for the trail.
-- * 'responseStatus' - The response status code.
-- * 'trailARN' - The specified trail ARN that has the event selectors.
mkGetEventSelectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEventSelectorsResponse
mkGetEventSelectorsResponse pResponseStatus_ =
  GetEventSelectorsResponse'
    { trailARN = Lude.Nothing,
      eventSelectors = Lude.Nothing,
      advancedEventSelectors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The specified trail ARN that has the event selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsTrailARN :: Lens.Lens' GetEventSelectorsResponse (Lude.Maybe Lude.Text)
gesrsTrailARN = Lens.lens (trailARN :: GetEventSelectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: GetEventSelectorsResponse)
{-# DEPRECATED gesrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The event selectors that are configured for the trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Lude.Maybe [EventSelector])
gesrsEventSelectors = Lens.lens (eventSelectors :: GetEventSelectorsResponse -> Lude.Maybe [EventSelector]) (\s a -> s {eventSelectors = a} :: GetEventSelectorsResponse)
{-# DEPRECATED gesrsEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsAdvancedEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Lude.Maybe [AdvancedEventSelector])
gesrsAdvancedEventSelectors = Lens.lens (advancedEventSelectors :: GetEventSelectorsResponse -> Lude.Maybe [AdvancedEventSelector]) (\s a -> s {advancedEventSelectors = a} :: GetEventSelectorsResponse)
{-# DEPRECATED gesrsAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrsResponseStatus :: Lens.Lens' GetEventSelectorsResponse Lude.Int
gesrsResponseStatus = Lens.lens (responseStatus :: GetEventSelectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEventSelectorsResponse)
{-# DEPRECATED gesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

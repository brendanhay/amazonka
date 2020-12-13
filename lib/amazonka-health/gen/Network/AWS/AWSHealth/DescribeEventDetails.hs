{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events. Information includes standard event data (Region, service, and so on, as returned by <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents> ), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included. To retrieve those, use the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities> operation.
--
-- If a specified event cannot be retrieved, an error message is returned for that event.
module Network.AWS.AWSHealth.DescribeEventDetails
  ( -- * Creating a request
    DescribeEventDetails (..),
    mkDescribeEventDetails,

    -- ** Request lenses
    dedEventARNs,
    dedLocale,

    -- * Destructuring the response
    DescribeEventDetailsResponse (..),
    mkDescribeEventDetailsResponse,

    -- ** Response lenses
    dedrsSuccessfulSet,
    dedrsFailedSet,
    dedrsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventDetails' smart constructor.
data DescribeEventDetails = DescribeEventDetails'
  { -- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
    eventARNs :: Lude.NonEmpty Lude.Text,
    -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventDetails' with the minimum fields required to make a request.
--
-- * 'eventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
mkDescribeEventDetails ::
  -- | 'eventARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeEventDetails
mkDescribeEventDetails pEventARNs_ =
  DescribeEventDetails'
    { eventARNs = pEventARNs_,
      locale = Lude.Nothing
    }

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- /Note:/ Consider using 'eventARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedEventARNs :: Lens.Lens' DescribeEventDetails (Lude.NonEmpty Lude.Text)
dedEventARNs = Lens.lens (eventARNs :: DescribeEventDetails -> Lude.NonEmpty Lude.Text) (\s a -> s {eventARNs = a} :: DescribeEventDetails)
{-# DEPRECATED dedEventARNs "Use generic-lens or generic-optics with 'eventARNs' instead." #-}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedLocale :: Lens.Lens' DescribeEventDetails (Lude.Maybe Lude.Text)
dedLocale = Lens.lens (locale :: DescribeEventDetails -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeEventDetails)
{-# DEPRECATED dedLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

instance Lude.AWSRequest DescribeEventDetails where
  type Rs DescribeEventDetails = DescribeEventDetailsResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventDetailsResponse'
            Lude.<$> (x Lude..?> "successfulSet" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedSet" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeEventDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventDetails where
  toJSON DescribeEventDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("eventArns" Lude..= eventARNs),
            ("locale" Lude..=) Lude.<$> locale
          ]
      )

instance Lude.ToPath DescribeEventDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventDetailsResponse' smart constructor.
data DescribeEventDetailsResponse = DescribeEventDetailsResponse'
  { -- | Information about the events that could be retrieved.
    successfulSet :: Lude.Maybe [EventDetails],
    -- | Error messages for any events that could not be retrieved.
    failedSet :: Lude.Maybe [EventDetailsErrorItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventDetailsResponse' with the minimum fields required to make a request.
--
-- * 'successfulSet' - Information about the events that could be retrieved.
-- * 'failedSet' - Error messages for any events that could not be retrieved.
-- * 'responseStatus' - The response status code.
mkDescribeEventDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventDetailsResponse
mkDescribeEventDetailsResponse pResponseStatus_ =
  DescribeEventDetailsResponse'
    { successfulSet = Lude.Nothing,
      failedSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the events that could be retrieved.
--
-- /Note:/ Consider using 'successfulSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrsSuccessfulSet :: Lens.Lens' DescribeEventDetailsResponse (Lude.Maybe [EventDetails])
dedrsSuccessfulSet = Lens.lens (successfulSet :: DescribeEventDetailsResponse -> Lude.Maybe [EventDetails]) (\s a -> s {successfulSet = a} :: DescribeEventDetailsResponse)
{-# DEPRECATED dedrsSuccessfulSet "Use generic-lens or generic-optics with 'successfulSet' instead." #-}

-- | Error messages for any events that could not be retrieved.
--
-- /Note:/ Consider using 'failedSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrsFailedSet :: Lens.Lens' DescribeEventDetailsResponse (Lude.Maybe [EventDetailsErrorItem])
dedrsFailedSet = Lens.lens (failedSet :: DescribeEventDetailsResponse -> Lude.Maybe [EventDetailsErrorItem]) (\s a -> s {failedSet = a} :: DescribeEventDetailsResponse)
{-# DEPRECATED dedrsFailedSet "Use generic-lens or generic-optics with 'failedSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedrsResponseStatus :: Lens.Lens' DescribeEventDetailsResponse Lude.Int
dedrsResponseStatus = Lens.lens (responseStatus :: DescribeEventDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventDetailsResponse)
{-# DEPRECATED dedrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

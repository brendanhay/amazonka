{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for one or more trails associated with the current region for your account.
module Network.AWS.CloudTrail.DescribeTrails
  ( -- * Creating a request
    DescribeTrails (..),
    mkDescribeTrails,

    -- ** Request lenses
    dtIncludeShadowTrails,
    dtTrailNameList,

    -- * Destructuring the response
    DescribeTrailsResponse (..),
    mkDescribeTrailsResponse,

    -- ** Response lenses
    dtrsTrailList,
    dtrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Returns information about the trail.
--
-- /See:/ 'mkDescribeTrails' smart constructor.
data DescribeTrails = DescribeTrails'
  { -- | Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region, or in the case of an organization trail, the replication of an organization trail in member accounts. If you do not include shadow trails, organization trails in a member account and region replication trails will not be returned. The default is true.
    includeShadowTrails :: Lude.Maybe Lude.Bool,
    -- | Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    -- If an empty list is specified, information for the trail in the current region is returned.
    --
    --     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.
    --
    --
    --     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
    trailNameList :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrails' with the minimum fields required to make a request.
--
-- * 'includeShadowTrails' - Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region, or in the case of an organization trail, the replication of an organization trail in member accounts. If you do not include shadow trails, organization trails in a member account and region replication trails will not be returned. The default is true.
-- * 'trailNameList' - Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- If an empty list is specified, information for the trail in the current region is returned.
--
--     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.
--
--
--     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
mkDescribeTrails ::
  DescribeTrails
mkDescribeTrails =
  DescribeTrails'
    { includeShadowTrails = Lude.Nothing,
      trailNameList = Lude.Nothing
    }

-- | Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region, or in the case of an organization trail, the replication of an organization trail in member accounts. If you do not include shadow trails, organization trails in a member account and region replication trails will not be returned. The default is true.
--
-- /Note:/ Consider using 'includeShadowTrails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtIncludeShadowTrails :: Lens.Lens' DescribeTrails (Lude.Maybe Lude.Bool)
dtIncludeShadowTrails = Lens.lens (includeShadowTrails :: DescribeTrails -> Lude.Maybe Lude.Bool) (\s a -> s {includeShadowTrails = a} :: DescribeTrails)
{-# DEPRECATED dtIncludeShadowTrails "Use generic-lens or generic-optics with 'includeShadowTrails' instead." #-}

-- | Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
-- If an empty list is specified, information for the trail in the current region is returned.
--
--     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.
--
--
--     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
--
--
--
-- /Note:/ Consider using 'trailNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrailNameList :: Lens.Lens' DescribeTrails (Lude.Maybe [Lude.Text])
dtTrailNameList = Lens.lens (trailNameList :: DescribeTrails -> Lude.Maybe [Lude.Text]) (\s a -> s {trailNameList = a} :: DescribeTrails)
{-# DEPRECATED dtTrailNameList "Use generic-lens or generic-optics with 'trailNameList' instead." #-}

instance Lude.AWSRequest DescribeTrails where
  type Rs DescribeTrails = DescribeTrailsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrailsResponse'
            Lude.<$> (x Lude..?> "trailList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeTrails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrails where
  toJSON DescribeTrails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeShadowTrails" Lude..=) Lude.<$> includeShadowTrails,
            ("trailNameList" Lude..=) Lude.<$> trailNameList
          ]
      )

instance Lude.ToPath DescribeTrails where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrails where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkDescribeTrailsResponse' smart constructor.
data DescribeTrailsResponse = DescribeTrailsResponse'
  { -- | The list of trail objects. Trail objects with string values are only returned if values for the objects exist in a trail's configuration. For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in results if a trail is configured to send SNS notifications. Similarly, @KMSKeyId@ only appears in results if a trail's log files are encrypted with AWS KMS-managed keys.
    trailList :: Lude.Maybe [Trail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrailsResponse' with the minimum fields required to make a request.
--
-- * 'trailList' - The list of trail objects. Trail objects with string values are only returned if values for the objects exist in a trail's configuration. For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in results if a trail is configured to send SNS notifications. Similarly, @KMSKeyId@ only appears in results if a trail's log files are encrypted with AWS KMS-managed keys.
-- * 'responseStatus' - The response status code.
mkDescribeTrailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrailsResponse
mkDescribeTrailsResponse pResponseStatus_ =
  DescribeTrailsResponse'
    { trailList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of trail objects. Trail objects with string values are only returned if values for the objects exist in a trail's configuration. For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in results if a trail is configured to send SNS notifications. Similarly, @KMSKeyId@ only appears in results if a trail's log files are encrypted with AWS KMS-managed keys.
--
-- /Note:/ Consider using 'trailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTrailList :: Lens.Lens' DescribeTrailsResponse (Lude.Maybe [Trail])
dtrsTrailList = Lens.lens (trailList :: DescribeTrailsResponse -> Lude.Maybe [Trail]) (\s a -> s {trailList = a} :: DescribeTrailsResponse)
{-# DEPRECATED dtrsTrailList "Use generic-lens or generic-optics with 'trailList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTrailsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTrailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrailsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

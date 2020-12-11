{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetInsightSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the Insights event selectors that you configured for your trail. @GetInsightSelectors@ shows if CloudTrail Insights event logging is enabled on the trail, and if it is, which insight types are enabled. If you run @GetInsightSelectors@ on a trail that does not have Insights events enabled, the operation throws the exception @InsightNotEnabledException@
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-insights-events-with-cloudtrail.html Logging CloudTrail Insights Events for Trails > in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.GetInsightSelectors
  ( -- * Creating a request
    GetInsightSelectors (..),
    mkGetInsightSelectors,

    -- ** Request lenses
    gisTrailName,

    -- * Destructuring the response
    GetInsightSelectorsResponse (..),
    mkGetInsightSelectorsResponse,

    -- ** Response lenses
    gisrsTrailARN,
    gisrsInsightSelectors,
    gisrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInsightSelectors' smart constructor.
newtype GetInsightSelectors = GetInsightSelectors'
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

-- | Creates a value of 'GetInsightSelectors' with the minimum fields required to make a request.
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
mkGetInsightSelectors ::
  -- | 'trailName'
  Lude.Text ->
  GetInsightSelectors
mkGetInsightSelectors pTrailName_ =
  GetInsightSelectors' {trailName = pTrailName_}

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
gisTrailName :: Lens.Lens' GetInsightSelectors Lude.Text
gisTrailName = Lens.lens (trailName :: GetInsightSelectors -> Lude.Text) (\s a -> s {trailName = a} :: GetInsightSelectors)
{-# DEPRECATED gisTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

instance Lude.AWSRequest GetInsightSelectors where
  type Rs GetInsightSelectors = GetInsightSelectorsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInsightSelectorsResponse'
            Lude.<$> (x Lude..?> "TrailARN")
            Lude.<*> (x Lude..?> "InsightSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsightSelectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetInsightSelectors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInsightSelectors where
  toJSON GetInsightSelectors' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TrailName" Lude..= trailName)])

instance Lude.ToPath GetInsightSelectors where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInsightSelectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInsightSelectorsResponse' smart constructor.
data GetInsightSelectorsResponse = GetInsightSelectorsResponse'
  { trailARN ::
      Lude.Maybe Lude.Text,
    insightSelectors ::
      Lude.Maybe [InsightSelector],
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

-- | Creates a value of 'GetInsightSelectorsResponse' with the minimum fields required to make a request.
--
-- * 'insightSelectors' - A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
-- * 'responseStatus' - The response status code.
-- * 'trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to get Insights selectors.
mkGetInsightSelectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightSelectorsResponse
mkGetInsightSelectorsResponse pResponseStatus_ =
  GetInsightSelectorsResponse'
    { trailARN = Lude.Nothing,
      insightSelectors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of a trail for which you want to get Insights selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsTrailARN :: Lens.Lens' GetInsightSelectorsResponse (Lude.Maybe Lude.Text)
gisrsTrailARN = Lens.lens (trailARN :: GetInsightSelectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: GetInsightSelectorsResponse)
{-# DEPRECATED gisrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsInsightSelectors :: Lens.Lens' GetInsightSelectorsResponse (Lude.Maybe [InsightSelector])
gisrsInsightSelectors = Lens.lens (insightSelectors :: GetInsightSelectorsResponse -> Lude.Maybe [InsightSelector]) (\s a -> s {insightSelectors = a} :: GetInsightSelectorsResponse)
{-# DEPRECATED gisrsInsightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetInsightSelectorsResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetInsightSelectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightSelectorsResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

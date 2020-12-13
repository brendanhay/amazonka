{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.PutInsightSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lets you enable Insights event logging by specifying the Insights selectors that you want to enable on an existing trail. You also use @PutInsightSelectors@ to turn off Insights event logging, by passing an empty list of insight types. In this release, only @ApiCallRateInsight@ is supported as an Insights selector.
module Network.AWS.CloudTrail.PutInsightSelectors
  ( -- * Creating a request
    PutInsightSelectors (..),
    mkPutInsightSelectors,

    -- ** Request lenses
    pisTrailName,
    pisInsightSelectors,

    -- * Destructuring the response
    PutInsightSelectorsResponse (..),
    mkPutInsightSelectorsResponse,

    -- ** Response lenses
    pisrsTrailARN,
    pisrsInsightSelectors,
    pisrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutInsightSelectors' smart constructor.
data PutInsightSelectors = PutInsightSelectors'
  { -- | The name of the CloudTrail trail for which you want to change or add Insights selectors.
    trailName :: Lude.Text,
    -- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
    insightSelectors :: [InsightSelector]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInsightSelectors' with the minimum fields required to make a request.
--
-- * 'trailName' - The name of the CloudTrail trail for which you want to change or add Insights selectors.
-- * 'insightSelectors' - A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
mkPutInsightSelectors ::
  -- | 'trailName'
  Lude.Text ->
  PutInsightSelectors
mkPutInsightSelectors pTrailName_ =
  PutInsightSelectors'
    { trailName = pTrailName_,
      insightSelectors = Lude.mempty
    }

-- | The name of the CloudTrail trail for which you want to change or add Insights selectors.
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisTrailName :: Lens.Lens' PutInsightSelectors Lude.Text
pisTrailName = Lens.lens (trailName :: PutInsightSelectors -> Lude.Text) (\s a -> s {trailName = a} :: PutInsightSelectors)
{-# DEPRECATED pisTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisInsightSelectors :: Lens.Lens' PutInsightSelectors [InsightSelector]
pisInsightSelectors = Lens.lens (insightSelectors :: PutInsightSelectors -> [InsightSelector]) (\s a -> s {insightSelectors = a} :: PutInsightSelectors)
{-# DEPRECATED pisInsightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead." #-}

instance Lude.AWSRequest PutInsightSelectors where
  type Rs PutInsightSelectors = PutInsightSelectorsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutInsightSelectorsResponse'
            Lude.<$> (x Lude..?> "TrailARN")
            Lude.<*> (x Lude..?> "InsightSelectors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutInsightSelectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutInsightSelectors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutInsightSelectors where
  toJSON PutInsightSelectors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TrailName" Lude..= trailName),
            Lude.Just ("InsightSelectors" Lude..= insightSelectors)
          ]
      )

instance Lude.ToPath PutInsightSelectors where
  toPath = Lude.const "/"

instance Lude.ToQuery PutInsightSelectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutInsightSelectorsResponse' smart constructor.
data PutInsightSelectorsResponse = PutInsightSelectorsResponse'
  { -- | The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
    trailARN :: Lude.Maybe Lude.Text,
    -- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
    insightSelectors :: Lude.Maybe [InsightSelector],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInsightSelectorsResponse' with the minimum fields required to make a request.
--
-- * 'trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
-- * 'insightSelectors' - A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
-- * 'responseStatus' - The response status code.
mkPutInsightSelectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutInsightSelectorsResponse
mkPutInsightSelectorsResponse pResponseStatus_ =
  PutInsightSelectorsResponse'
    { trailARN = Lude.Nothing,
      insightSelectors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrsTrailARN :: Lens.Lens' PutInsightSelectorsResponse (Lude.Maybe Lude.Text)
pisrsTrailARN = Lens.lens (trailARN :: PutInsightSelectorsResponse -> Lude.Maybe Lude.Text) (\s a -> s {trailARN = a} :: PutInsightSelectorsResponse)
{-# DEPRECATED pisrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrsInsightSelectors :: Lens.Lens' PutInsightSelectorsResponse (Lude.Maybe [InsightSelector])
pisrsInsightSelectors = Lens.lens (insightSelectors :: PutInsightSelectorsResponse -> Lude.Maybe [InsightSelector]) (\s a -> s {insightSelectors = a} :: PutInsightSelectorsResponse)
{-# DEPRECATED pisrsInsightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrsResponseStatus :: Lens.Lens' PutInsightSelectorsResponse Lude.Int
pisrsResponseStatus = Lens.lens (responseStatus :: PutInsightSelectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutInsightSelectorsResponse)
{-# DEPRECATED pisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

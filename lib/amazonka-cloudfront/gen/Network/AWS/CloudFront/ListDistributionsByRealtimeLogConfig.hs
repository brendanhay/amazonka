{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distributions that have a cache behavior that’s associated with the specified real-time log configuration.
--
-- You can specify the real-time log configuration by its name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to list distributions for.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
  ( -- * Creating a request
    ListDistributionsByRealtimeLogConfig (..),
    mkListDistributionsByRealtimeLogConfig,

    -- ** Request lenses
    ldbrlcRealtimeLogConfigName,
    ldbrlcRealtimeLogConfigARN,
    ldbrlcMarker,
    ldbrlcMaxItems,

    -- * Destructuring the response
    ListDistributionsByRealtimeLogConfigResponse (..),
    mkListDistributionsByRealtimeLogConfigResponse,

    -- ** Response lenses
    ldbrlcrsDistributionList,
    ldbrlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDistributionsByRealtimeLogConfig' smart constructor.
data ListDistributionsByRealtimeLogConfig = ListDistributionsByRealtimeLogConfig'
  { realtimeLogConfigName ::
      Lude.Maybe
        Lude.Text,
    realtimeLogConfigARN ::
      Lude.Maybe
        Lude.Text,
    marker ::
      Lude.Maybe
        Lude.Text,
    maxItems ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByRealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of distributions that you want in the response.
-- * 'realtimeLogConfigARN' - The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
-- * 'realtimeLogConfigName' - The name of the real-time log configuration whose associated distributions you want to list.
mkListDistributionsByRealtimeLogConfig ::
  ListDistributionsByRealtimeLogConfig
mkListDistributionsByRealtimeLogConfig =
  ListDistributionsByRealtimeLogConfig'
    { realtimeLogConfigName =
        Lude.Nothing,
      realtimeLogConfigARN = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigName :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Lude.Maybe Lude.Text)
ldbrlcRealtimeLogConfigName = Lens.lens (realtimeLogConfigName :: ListDistributionsByRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {realtimeLogConfigName = a} :: ListDistributionsByRealtimeLogConfig)
{-# DEPRECATED ldbrlcRealtimeLogConfigName "Use generic-lens or generic-optics with 'realtimeLogConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigARN :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Lude.Maybe Lude.Text)
ldbrlcRealtimeLogConfigARN = Lens.lens (realtimeLogConfigARN :: ListDistributionsByRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {realtimeLogConfigARN = a} :: ListDistributionsByRealtimeLogConfig)
{-# DEPRECATED ldbrlcRealtimeLogConfigARN "Use generic-lens or generic-optics with 'realtimeLogConfigARN' instead." #-}

-- | Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMarker :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Lude.Maybe Lude.Text)
ldbrlcMarker = Lens.lens (marker :: ListDistributionsByRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDistributionsByRealtimeLogConfig)
{-# DEPRECATED ldbrlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMaxItems :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Lude.Maybe Lude.Text)
ldbrlcMaxItems = Lens.lens (maxItems :: ListDistributionsByRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListDistributionsByRealtimeLogConfig)
{-# DEPRECATED ldbrlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListDistributionsByRealtimeLogConfig where
  type
    Rs ListDistributionsByRealtimeLogConfig =
      ListDistributionsByRealtimeLogConfigResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListDistributionsByRealtimeLogConfigResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement ListDistributionsByRealtimeLogConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ListDistributionsByRealtimeLogConfigRequest"

instance Lude.ToHeaders ListDistributionsByRealtimeLogConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDistributionsByRealtimeLogConfig where
  toPath = Lude.const "/2020-05-31/distributionsByRealtimeLogConfig/"

instance Lude.ToQuery ListDistributionsByRealtimeLogConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML ListDistributionsByRealtimeLogConfig where
  toXML ListDistributionsByRealtimeLogConfig' {..} =
    Lude.mconcat
      [ "RealtimeLogConfigName" Lude.@= realtimeLogConfigName,
        "RealtimeLogConfigArn" Lude.@= realtimeLogConfigARN,
        "Marker" Lude.@= marker,
        "MaxItems" Lude.@= maxItems
      ]

-- | /See:/ 'mkListDistributionsByRealtimeLogConfigResponse' smart constructor.
data ListDistributionsByRealtimeLogConfigResponse = ListDistributionsByRealtimeLogConfigResponse'
  { distributionList ::
      Lude.Maybe
        DistributionList,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDistributionsByRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- * 'distributionList' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkListDistributionsByRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDistributionsByRealtimeLogConfigResponse
mkListDistributionsByRealtimeLogConfigResponse pResponseStatus_ =
  ListDistributionsByRealtimeLogConfigResponse'
    { distributionList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrsDistributionList :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse (Lude.Maybe DistributionList)
ldbrlcrsDistributionList = Lens.lens (distributionList :: ListDistributionsByRealtimeLogConfigResponse -> Lude.Maybe DistributionList) (\s a -> s {distributionList = a} :: ListDistributionsByRealtimeLogConfigResponse)
{-# DEPRECATED ldbrlcrsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrsResponseStatus :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse Lude.Int
ldbrlcrsResponseStatus = Lens.lens (responseStatus :: ListDistributionsByRealtimeLogConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDistributionsByRealtimeLogConfigResponse)
{-# DEPRECATED ldbrlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

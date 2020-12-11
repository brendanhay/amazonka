{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListLoggingConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'LoggingConfiguration' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListLoggingConfigurations
  ( -- * Creating a request
    ListLoggingConfigurations (..),
    mkListLoggingConfigurations,

    -- ** Request lenses
    llcNextMarker,
    llcLimit,

    -- * Destructuring the response
    ListLoggingConfigurationsResponse (..),
    mkListLoggingConfigurationsResponse,

    -- ** Response lenses
    llcrsNextMarker,
    llcrsLoggingConfigurations,
    llcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkListLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLoggingConfigurations' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
mkListLoggingConfigurations ::
  ListLoggingConfigurations
mkListLoggingConfigurations =
  ListLoggingConfigurations'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcNextMarker :: Lens.Lens' ListLoggingConfigurations (Lude.Maybe Lude.Text)
llcNextMarker = Lens.lens (nextMarker :: ListLoggingConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListLoggingConfigurations)
{-# DEPRECATED llcNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcLimit :: Lens.Lens' ListLoggingConfigurations (Lude.Maybe Lude.Natural)
llcLimit = Lens.lens (limit :: ListLoggingConfigurations -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListLoggingConfigurations)
{-# DEPRECATED llcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListLoggingConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. llcrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. llcrsLoggingConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llcNextMarker Lens..~ rs Lens.^. llcrsNextMarker

instance Lude.AWSRequest ListLoggingConfigurations where
  type
    Rs ListLoggingConfigurations =
      ListLoggingConfigurationsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLoggingConfigurationsResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "LoggingConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLoggingConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.ListLoggingConfigurations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLoggingConfigurations where
  toJSON ListLoggingConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListLoggingConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLoggingConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    loggingConfigurations ::
      Lude.Maybe
        [LoggingConfiguration],
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

-- | Creates a value of 'ListLoggingConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'loggingConfigurations' - An array of 'LoggingConfiguration' objects.
-- * 'nextMarker' - If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListLoggingConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLoggingConfigurationsResponse
mkListLoggingConfigurationsResponse pResponseStatus_ =
  ListLoggingConfigurationsResponse'
    { nextMarker = Lude.Nothing,
      loggingConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsNextMarker :: Lens.Lens' ListLoggingConfigurationsResponse (Lude.Maybe Lude.Text)
llcrsNextMarker = Lens.lens (nextMarker :: ListLoggingConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListLoggingConfigurationsResponse)
{-# DEPRECATED llcrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of 'LoggingConfiguration' objects.
--
-- /Note:/ Consider using 'loggingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsLoggingConfigurations :: Lens.Lens' ListLoggingConfigurationsResponse (Lude.Maybe [LoggingConfiguration])
llcrsLoggingConfigurations = Lens.lens (loggingConfigurations :: ListLoggingConfigurationsResponse -> Lude.Maybe [LoggingConfiguration]) (\s a -> s {loggingConfigurations = a} :: ListLoggingConfigurationsResponse)
{-# DEPRECATED llcrsLoggingConfigurations "Use generic-lens or generic-optics with 'loggingConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrsResponseStatus :: Lens.Lens' ListLoggingConfigurationsResponse Lude.Int
llcrsResponseStatus = Lens.lens (responseStatus :: ListLoggingConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLoggingConfigurationsResponse)
{-# DEPRECATED llcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

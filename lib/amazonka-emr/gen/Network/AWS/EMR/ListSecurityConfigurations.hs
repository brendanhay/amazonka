{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListSecurityConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the security configurations visible to this account, providing their creation dates and times, and their names. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListSecurityConfigurations calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListSecurityConfigurations
  ( -- * Creating a request
    ListSecurityConfigurations (..),
    mkListSecurityConfigurations,

    -- ** Request lenses
    lscMarker,

    -- * Destructuring the response
    ListSecurityConfigurationsResponse (..),
    mkListSecurityConfigurationsResponse,

    -- ** Response lenses
    lscrsSecurityConfigurations,
    lscrsMarker,
    lscrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSecurityConfigurations' smart constructor.
newtype ListSecurityConfigurations = ListSecurityConfigurations'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityConfigurations' with the minimum fields required to make a request.
--
-- * 'marker' - The pagination token that indicates the set of results to retrieve.
mkListSecurityConfigurations ::
  ListSecurityConfigurations
mkListSecurityConfigurations =
  ListSecurityConfigurations' {marker = Lude.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMarker :: Lens.Lens' ListSecurityConfigurations (Lude.Maybe Lude.Text)
lscMarker = Lens.lens (marker :: ListSecurityConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSecurityConfigurations)
{-# DEPRECATED lscMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListSecurityConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. lscrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lscrsSecurityConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lscMarker Lens..~ rs Lens.^. lscrsMarker

instance Lude.AWSRequest ListSecurityConfigurations where
  type
    Rs ListSecurityConfigurations =
      ListSecurityConfigurationsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecurityConfigurationsResponse'
            Lude.<$> (x Lude..?> "SecurityConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecurityConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListSecurityConfigurations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSecurityConfigurations where
  toJSON ListSecurityConfigurations' {..} =
    Lude.object (Lude.catMaybes [("Marker" Lude..=) Lude.<$> marker])

instance Lude.ToPath ListSecurityConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSecurityConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSecurityConfigurationsResponse' smart constructor.
data ListSecurityConfigurationsResponse = ListSecurityConfigurationsResponse'
  { -- | The creation date and time, and name, of each security configuration.
    securityConfigurations :: Lude.Maybe [SecurityConfigurationSummary],
    -- | A pagination token that indicates the next set of results to retrieve. Include the marker in the next ListSecurityConfiguration call to retrieve the next page of results, if required.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'securityConfigurations' - The creation date and time, and name, of each security configuration.
-- * 'marker' - A pagination token that indicates the next set of results to retrieve. Include the marker in the next ListSecurityConfiguration call to retrieve the next page of results, if required.
-- * 'responseStatus' - The response status code.
mkListSecurityConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecurityConfigurationsResponse
mkListSecurityConfigurationsResponse pResponseStatus_ =
  ListSecurityConfigurationsResponse'
    { securityConfigurations =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The creation date and time, and name, of each security configuration.
--
-- /Note:/ Consider using 'securityConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsSecurityConfigurations :: Lens.Lens' ListSecurityConfigurationsResponse (Lude.Maybe [SecurityConfigurationSummary])
lscrsSecurityConfigurations = Lens.lens (securityConfigurations :: ListSecurityConfigurationsResponse -> Lude.Maybe [SecurityConfigurationSummary]) (\s a -> s {securityConfigurations = a} :: ListSecurityConfigurationsResponse)
{-# DEPRECATED lscrsSecurityConfigurations "Use generic-lens or generic-optics with 'securityConfigurations' instead." #-}

-- | A pagination token that indicates the next set of results to retrieve. Include the marker in the next ListSecurityConfiguration call to retrieve the next page of results, if required.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsMarker :: Lens.Lens' ListSecurityConfigurationsResponse (Lude.Maybe Lude.Text)
lscrsMarker = Lens.lens (marker :: ListSecurityConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSecurityConfigurationsResponse)
{-# DEPRECATED lscrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsResponseStatus :: Lens.Lens' ListSecurityConfigurationsResponse Lude.Int
lscrsResponseStatus = Lens.lens (responseStatus :: ListSecurityConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecurityConfigurationsResponse)
{-# DEPRECATED lscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

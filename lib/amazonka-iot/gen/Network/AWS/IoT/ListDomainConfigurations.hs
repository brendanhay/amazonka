{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListDomainConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of domain configurations for the user. This list is sorted alphabetically by domain configuration name.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDomainConfigurations
  ( -- * Creating a request
    ListDomainConfigurations (..),
    mkListDomainConfigurations,

    -- ** Request lenses
    ldcMarker,
    ldcServiceType,
    ldcPageSize,

    -- * Destructuring the response
    ListDomainConfigurationsResponse (..),
    mkListDomainConfigurationsResponse,

    -- ** Response lenses
    ldcrsDomainConfigurations,
    ldcrsNextMarker,
    ldcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDomainConfigurations' smart constructor.
data ListDomainConfigurations = ListDomainConfigurations'
  { marker ::
      Lude.Maybe Lude.Text,
    serviceType :: Lude.Maybe ServiceType,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainConfigurations' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results.
-- * 'pageSize' - The result page size.
-- * 'serviceType' - The type of service delivered by the endpoint.
mkListDomainConfigurations ::
  ListDomainConfigurations
mkListDomainConfigurations =
  ListDomainConfigurations'
    { marker = Lude.Nothing,
      serviceType = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMarker :: Lens.Lens' ListDomainConfigurations (Lude.Maybe Lude.Text)
ldcMarker = Lens.lens (marker :: ListDomainConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDomainConfigurations)
{-# DEPRECATED ldcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcServiceType :: Lens.Lens' ListDomainConfigurations (Lude.Maybe ServiceType)
ldcServiceType = Lens.lens (serviceType :: ListDomainConfigurations -> Lude.Maybe ServiceType) (\s a -> s {serviceType = a} :: ListDomainConfigurations)
{-# DEPRECATED ldcServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcPageSize :: Lens.Lens' ListDomainConfigurations (Lude.Maybe Lude.Natural)
ldcPageSize = Lens.lens (pageSize :: ListDomainConfigurations -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListDomainConfigurations)
{-# DEPRECATED ldcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListDomainConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. ldcrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ldcrsDomainConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldcMarker Lens..~ rs Lens.^. ldcrsNextMarker

instance Lude.AWSRequest ListDomainConfigurations where
  type Rs ListDomainConfigurations = ListDomainConfigurationsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDomainConfigurationsResponse'
            Lude.<$> (x Lude..?> "domainConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDomainConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDomainConfigurations where
  toPath = Lude.const "/domainConfigurations"

instance Lude.ToQuery ListDomainConfigurations where
  toQuery ListDomainConfigurations' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "serviceType" Lude.=: serviceType,
        "pageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkListDomainConfigurationsResponse' smart constructor.
data ListDomainConfigurationsResponse = ListDomainConfigurationsResponse'
  { domainConfigurations ::
      Lude.Maybe
        [DomainConfigurationSummary],
    nextMarker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDomainConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'domainConfigurations' - A list of objects that contain summary information about the user's domain configurations.
-- * 'nextMarker' - The marker for the next set of results.
-- * 'responseStatus' - The response status code.
mkListDomainConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainConfigurationsResponse
mkListDomainConfigurationsResponse pResponseStatus_ =
  ListDomainConfigurationsResponse'
    { domainConfigurations =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that contain summary information about the user's domain configurations.
--
-- /Note:/ Consider using 'domainConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsDomainConfigurations :: Lens.Lens' ListDomainConfigurationsResponse (Lude.Maybe [DomainConfigurationSummary])
ldcrsDomainConfigurations = Lens.lens (domainConfigurations :: ListDomainConfigurationsResponse -> Lude.Maybe [DomainConfigurationSummary]) (\s a -> s {domainConfigurations = a} :: ListDomainConfigurationsResponse)
{-# DEPRECATED ldcrsDomainConfigurations "Use generic-lens or generic-optics with 'domainConfigurations' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsNextMarker :: Lens.Lens' ListDomainConfigurationsResponse (Lude.Maybe Lude.Text)
ldcrsNextMarker = Lens.lens (nextMarker :: ListDomainConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListDomainConfigurationsResponse)
{-# DEPRECATED ldcrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrsResponseStatus :: Lens.Lens' ListDomainConfigurationsResponse Lude.Int
ldcrsResponseStatus = Lens.lens (responseStatus :: ListDomainConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainConfigurationsResponse)
{-# DEPRECATED ldcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

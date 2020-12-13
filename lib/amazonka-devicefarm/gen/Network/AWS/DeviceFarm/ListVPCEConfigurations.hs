{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListVPCEConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Virtual Private Cloud (VPC) endpoint configurations in the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListVPCEConfigurations
  ( -- * Creating a request
    ListVPCEConfigurations (..),
    mkListVPCEConfigurations,

    -- ** Request lenses
    lvecNextToken,
    lvecMaxResults,

    -- * Destructuring the response
    ListVPCEConfigurationsResponse (..),
    mkListVPCEConfigurationsResponse,

    -- ** Response lenses
    lvecrsNextToken,
    lvecrsVpceConfigurations,
    lvecrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListVPCEConfigurations' smart constructor.
data ListVPCEConfigurations = ListVPCEConfigurations'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An integer that specifies the maximum number of items you want to return in the API response.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVPCEConfigurations' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'maxResults' - An integer that specifies the maximum number of items you want to return in the API response.
mkListVPCEConfigurations ::
  ListVPCEConfigurations
mkListVPCEConfigurations =
  ListVPCEConfigurations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvecNextToken :: Lens.Lens' ListVPCEConfigurations (Lude.Maybe Lude.Text)
lvecNextToken = Lens.lens (nextToken :: ListVPCEConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVPCEConfigurations)
{-# DEPRECATED lvecNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvecMaxResults :: Lens.Lens' ListVPCEConfigurations (Lude.Maybe Lude.Int)
lvecMaxResults = Lens.lens (maxResults :: ListVPCEConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListVPCEConfigurations)
{-# DEPRECATED lvecMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListVPCEConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. lvecrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lvecrsVpceConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lvecNextToken Lens..~ rs Lens.^. lvecrsNextToken

instance Lude.AWSRequest ListVPCEConfigurations where
  type Rs ListVPCEConfigurations = ListVPCEConfigurationsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVPCEConfigurationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "vpceConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVPCEConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListVPCEConfigurations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVPCEConfigurations where
  toJSON ListVPCEConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListVPCEConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVPCEConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListVPCEConfigurationsResponse' smart constructor.
data ListVPCEConfigurationsResponse = ListVPCEConfigurationsResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @VPCEConfiguration@ objects that contain information about your VPC endpoint configuration.
    vpceConfigurations :: Lude.Maybe [VPCEConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVPCEConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'vpceConfigurations' - An array of @VPCEConfiguration@ objects that contain information about your VPC endpoint configuration.
-- * 'responseStatus' - The response status code.
mkListVPCEConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVPCEConfigurationsResponse
mkListVPCEConfigurationsResponse pResponseStatus_ =
  ListVPCEConfigurationsResponse'
    { nextToken = Lude.Nothing,
      vpceConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvecrsNextToken :: Lens.Lens' ListVPCEConfigurationsResponse (Lude.Maybe Lude.Text)
lvecrsNextToken = Lens.lens (nextToken :: ListVPCEConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVPCEConfigurationsResponse)
{-# DEPRECATED lvecrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @VPCEConfiguration@ objects that contain information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvecrsVpceConfigurations :: Lens.Lens' ListVPCEConfigurationsResponse (Lude.Maybe [VPCEConfiguration])
lvecrsVpceConfigurations = Lens.lens (vpceConfigurations :: ListVPCEConfigurationsResponse -> Lude.Maybe [VPCEConfiguration]) (\s a -> s {vpceConfigurations = a} :: ListVPCEConfigurationsResponse)
{-# DEPRECATED lvecrsVpceConfigurations "Use generic-lens or generic-optics with 'vpceConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvecrsResponseStatus :: Lens.Lens' ListVPCEConfigurationsResponse Lude.Int
lvecrsResponseStatus = Lens.lens (responseStatus :: ListVPCEConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVPCEConfigurationsResponse)
{-# DEPRECATED lvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

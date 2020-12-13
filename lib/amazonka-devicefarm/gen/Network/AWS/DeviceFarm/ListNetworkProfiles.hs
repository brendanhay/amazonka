{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListNetworkProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available network profiles.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListNetworkProfiles
  ( -- * Creating a request
    ListNetworkProfiles (..),
    mkListNetworkProfiles,

    -- ** Request lenses
    lnpArn,
    lnpNextToken,
    lnpType,

    -- * Destructuring the response
    ListNetworkProfilesResponse (..),
    mkListNetworkProfilesResponse,

    -- ** Response lenses
    lnprsNetworkProfiles,
    lnprsNextToken,
    lnprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListNetworkProfiles' smart constructor.
data ListNetworkProfiles = ListNetworkProfiles'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The type of network profile to return information about. Valid values are listed here.
    type' :: Lude.Maybe NetworkProfileType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNetworkProfiles' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'type'' - The type of network profile to return information about. Valid values are listed here.
mkListNetworkProfiles ::
  -- | 'arn'
  Lude.Text ->
  ListNetworkProfiles
mkListNetworkProfiles pArn_ =
  ListNetworkProfiles'
    { arn = pArn_,
      nextToken = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpArn :: Lens.Lens' ListNetworkProfiles Lude.Text
lnpArn = Lens.lens (arn :: ListNetworkProfiles -> Lude.Text) (\s a -> s {arn = a} :: ListNetworkProfiles)
{-# DEPRECATED lnpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpNextToken :: Lens.Lens' ListNetworkProfiles (Lude.Maybe Lude.Text)
lnpNextToken = Lens.lens (nextToken :: ListNetworkProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNetworkProfiles)
{-# DEPRECATED lnpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of network profile to return information about. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnpType :: Lens.Lens' ListNetworkProfiles (Lude.Maybe NetworkProfileType)
lnpType = Lens.lens (type' :: ListNetworkProfiles -> Lude.Maybe NetworkProfileType) (\s a -> s {type' = a} :: ListNetworkProfiles)
{-# DEPRECATED lnpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Page.AWSPager ListNetworkProfiles where
  page rq rs
    | Page.stop (rs Lens.^. lnprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lnprsNetworkProfiles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lnpNextToken Lens..~ rs Lens.^. lnprsNextToken

instance Lude.AWSRequest ListNetworkProfiles where
  type Rs ListNetworkProfiles = ListNetworkProfilesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNetworkProfilesResponse'
            Lude.<$> (x Lude..?> "networkProfiles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNetworkProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListNetworkProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListNetworkProfiles where
  toJSON ListNetworkProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("type" Lude..=) Lude.<$> type'
          ]
      )

instance Lude.ToPath ListNetworkProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery ListNetworkProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListNetworkProfilesResponse' smart constructor.
data ListNetworkProfilesResponse = ListNetworkProfilesResponse'
  { -- | A list of the available network profiles.
    networkProfiles :: Lude.Maybe [NetworkProfile],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNetworkProfilesResponse' with the minimum fields required to make a request.
--
-- * 'networkProfiles' - A list of the available network profiles.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListNetworkProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNetworkProfilesResponse
mkListNetworkProfilesResponse pResponseStatus_ =
  ListNetworkProfilesResponse'
    { networkProfiles = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the available network profiles.
--
-- /Note:/ Consider using 'networkProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprsNetworkProfiles :: Lens.Lens' ListNetworkProfilesResponse (Lude.Maybe [NetworkProfile])
lnprsNetworkProfiles = Lens.lens (networkProfiles :: ListNetworkProfilesResponse -> Lude.Maybe [NetworkProfile]) (\s a -> s {networkProfiles = a} :: ListNetworkProfilesResponse)
{-# DEPRECATED lnprsNetworkProfiles "Use generic-lens or generic-optics with 'networkProfiles' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprsNextToken :: Lens.Lens' ListNetworkProfilesResponse (Lude.Maybe Lude.Text)
lnprsNextToken = Lens.lens (nextToken :: ListNetworkProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNetworkProfilesResponse)
{-# DEPRECATED lnprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnprsResponseStatus :: Lens.Lens' ListNetworkProfilesResponse Lude.Int
lnprsResponseStatus = Lens.lens (responseStatus :: ListNetworkProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNetworkProfilesResponse)
{-# DEPRECATED lnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

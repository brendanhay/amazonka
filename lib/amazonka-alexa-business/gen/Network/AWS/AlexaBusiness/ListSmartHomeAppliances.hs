{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSmartHomeAppliances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the smart home appliances associated with a room.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSmartHomeAppliances
  ( -- * Creating a request
    ListSmartHomeAppliances (..),
    mkListSmartHomeAppliances,

    -- ** Request lenses
    lshaNextToken,
    lshaMaxResults,
    lshaRoomARN,

    -- * Destructuring the response
    ListSmartHomeAppliancesResponse (..),
    mkListSmartHomeAppliancesResponse,

    -- ** Response lenses
    lsharsSmartHomeAppliances,
    lsharsNextToken,
    lsharsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSmartHomeAppliances' smart constructor.
data ListSmartHomeAppliances = ListSmartHomeAppliances'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    roomARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSmartHomeAppliances' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of appliances to be returned, per paginated calls.
-- * 'nextToken' - The tokens used for pagination.
-- * 'roomARN' - The room that the appliances are associated with.
mkListSmartHomeAppliances ::
  -- | 'roomARN'
  Lude.Text ->
  ListSmartHomeAppliances
mkListSmartHomeAppliances pRoomARN_ =
  ListSmartHomeAppliances'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      roomARN = pRoomARN_
    }

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaNextToken :: Lens.Lens' ListSmartHomeAppliances (Lude.Maybe Lude.Text)
lshaNextToken = Lens.lens (nextToken :: ListSmartHomeAppliances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSmartHomeAppliances)
{-# DEPRECATED lshaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of appliances to be returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaMaxResults :: Lens.Lens' ListSmartHomeAppliances (Lude.Maybe Lude.Natural)
lshaMaxResults = Lens.lens (maxResults :: ListSmartHomeAppliances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSmartHomeAppliances)
{-# DEPRECATED lshaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The room that the appliances are associated with.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaRoomARN :: Lens.Lens' ListSmartHomeAppliances Lude.Text
lshaRoomARN = Lens.lens (roomARN :: ListSmartHomeAppliances -> Lude.Text) (\s a -> s {roomARN = a} :: ListSmartHomeAppliances)
{-# DEPRECATED lshaRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Page.AWSPager ListSmartHomeAppliances where
  page rq rs
    | Page.stop (rs Lens.^. lsharsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsharsSmartHomeAppliances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lshaNextToken Lens..~ rs Lens.^. lsharsNextToken

instance Lude.AWSRequest ListSmartHomeAppliances where
  type Rs ListSmartHomeAppliances = ListSmartHomeAppliancesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSmartHomeAppliancesResponse'
            Lude.<$> (x Lude..?> "SmartHomeAppliances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSmartHomeAppliances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListSmartHomeAppliances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSmartHomeAppliances where
  toJSON ListSmartHomeAppliances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("RoomArn" Lude..= roomARN)
          ]
      )

instance Lude.ToPath ListSmartHomeAppliances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSmartHomeAppliances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSmartHomeAppliancesResponse' smart constructor.
data ListSmartHomeAppliancesResponse = ListSmartHomeAppliancesResponse'
  { smartHomeAppliances ::
      Lude.Maybe
        [SmartHomeAppliance],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListSmartHomeAppliancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The tokens used for pagination.
-- * 'responseStatus' - The response status code.
-- * 'smartHomeAppliances' - The smart home appliances.
mkListSmartHomeAppliancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSmartHomeAppliancesResponse
mkListSmartHomeAppliancesResponse pResponseStatus_ =
  ListSmartHomeAppliancesResponse'
    { smartHomeAppliances =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The smart home appliances.
--
-- /Note:/ Consider using 'smartHomeAppliances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharsSmartHomeAppliances :: Lens.Lens' ListSmartHomeAppliancesResponse (Lude.Maybe [SmartHomeAppliance])
lsharsSmartHomeAppliances = Lens.lens (smartHomeAppliances :: ListSmartHomeAppliancesResponse -> Lude.Maybe [SmartHomeAppliance]) (\s a -> s {smartHomeAppliances = a} :: ListSmartHomeAppliancesResponse)
{-# DEPRECATED lsharsSmartHomeAppliances "Use generic-lens or generic-optics with 'smartHomeAppliances' instead." #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharsNextToken :: Lens.Lens' ListSmartHomeAppliancesResponse (Lude.Maybe Lude.Text)
lsharsNextToken = Lens.lens (nextToken :: ListSmartHomeAppliancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSmartHomeAppliancesResponse)
{-# DEPRECATED lsharsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharsResponseStatus :: Lens.Lens' ListSmartHomeAppliancesResponse Lude.Int
lsharsResponseStatus = Lens.lens (responseStatus :: ListSmartHomeAppliancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSmartHomeAppliancesResponse)
{-# DEPRECATED lsharsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

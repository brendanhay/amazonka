{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the usage data of a usage plan in a specified time interval.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsage
  ( -- * Creating a request
    GetUsage (..),
    mkGetUsage,

    -- ** Request lenses
    guKeyId,
    guLimit,
    guPosition,
    guUsagePlanId,
    guStartDate,
    guEndDate,

    -- * Destructuring the response
    Usage (..),
    mkUsage,

    -- ** Response lenses
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to get the usage data of a usage plan in a specified time interval.
--
-- /See:/ 'mkGetUsage' smart constructor.
data GetUsage = GetUsage'
  { keyId :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Int,
    position :: Lude.Maybe Lude.Text,
    usagePlanId :: Lude.Text,
    startDate :: Lude.Text,
    endDate :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsage' with the minimum fields required to make a request.
--
-- * 'endDate' - [Required] The ending date (e.g., 2016-12-31) of the usage data.
-- * 'keyId' - The Id of the API key associated with the resultant usage data.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
-- * 'startDate' - [Required] The starting date (e.g., 2016-01-01) of the usage data.
-- * 'usagePlanId' - [Required] The Id of the usage plan associated with the usage data.
mkGetUsage ::
  -- | 'usagePlanId'
  Lude.Text ->
  -- | 'startDate'
  Lude.Text ->
  -- | 'endDate'
  Lude.Text ->
  GetUsage
mkGetUsage pUsagePlanId_ pStartDate_ pEndDate_ =
  GetUsage'
    { keyId = Lude.Nothing,
      limit = Lude.Nothing,
      position = Lude.Nothing,
      usagePlanId = pUsagePlanId_,
      startDate = pStartDate_,
      endDate = pEndDate_
    }

-- | The Id of the API key associated with the resultant usage data.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guKeyId :: Lens.Lens' GetUsage (Lude.Maybe Lude.Text)
guKeyId = Lens.lens (keyId :: GetUsage -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GetUsage)
{-# DEPRECATED guKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guLimit :: Lens.Lens' GetUsage (Lude.Maybe Lude.Int)
guLimit = Lens.lens (limit :: GetUsage -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetUsage)
{-# DEPRECATED guLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guPosition :: Lens.Lens' GetUsage (Lude.Maybe Lude.Text)
guPosition = Lens.lens (position :: GetUsage -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetUsage)
{-# DEPRECATED guPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | [Required] The Id of the usage plan associated with the usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guUsagePlanId :: Lens.Lens' GetUsage Lude.Text
guUsagePlanId = Lens.lens (usagePlanId :: GetUsage -> Lude.Text) (\s a -> s {usagePlanId = a} :: GetUsage)
{-# DEPRECATED guUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

-- | [Required] The starting date (e.g., 2016-01-01) of the usage data.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guStartDate :: Lens.Lens' GetUsage Lude.Text
guStartDate = Lens.lens (startDate :: GetUsage -> Lude.Text) (\s a -> s {startDate = a} :: GetUsage)
{-# DEPRECATED guStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | [Required] The ending date (e.g., 2016-12-31) of the usage data.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guEndDate :: Lens.Lens' GetUsage Lude.Text
guEndDate = Lens.lens (endDate :: GetUsage -> Lude.Text) (\s a -> s {endDate = a} :: GetUsage)
{-# DEPRECATED guEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

instance Page.AWSPager GetUsage where
  page rq rs
    | Page.stop (rs Lens.^. uPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. uItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& guPosition Lens..~ rs Lens.^. uPosition

instance Lude.AWSRequest GetUsage where
  type Rs GetUsage = Usage
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetUsage where
  toPath GetUsage' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId, "/usage"]

instance Lude.ToQuery GetUsage where
  toQuery GetUsage' {..} =
    Lude.mconcat
      [ "keyId" Lude.=: keyId,
        "limit" Lude.=: limit,
        "position" Lude.=: position,
        "startDate" Lude.=: startDate,
        "endDate" Lude.=: endDate
      ]

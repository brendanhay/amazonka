{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSetLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetReusableDelegationSetLimit
  ( -- * Creating a request
    GetReusableDelegationSetLimit (..),
    mkGetReusableDelegationSetLimit,

    -- ** Request lenses
    grdslType,
    grdslDelegationSetId,

    -- * Destructuring the response
    GetReusableDelegationSetLimitResponse (..),
    mkGetReusableDelegationSetLimitResponse,

    -- ** Response lenses
    grdslrsResponseStatus,
    grdslrsLimit,
    grdslrsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetReusableDelegationSetLimit' smart constructor.
data GetReusableDelegationSetLimit = GetReusableDelegationSetLimit'
  { type' ::
      ReusableDelegationSetLimitType,
    delegationSetId :: ResourceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReusableDelegationSetLimit' with the minimum fields required to make a request.
--
-- * 'delegationSetId' - The ID of the delegation set that you want to get the limit for.
-- * 'type'' - Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
mkGetReusableDelegationSetLimit ::
  -- | 'type''
  ReusableDelegationSetLimitType ->
  -- | 'delegationSetId'
  ResourceId ->
  GetReusableDelegationSetLimit
mkGetReusableDelegationSetLimit pType_ pDelegationSetId_ =
  GetReusableDelegationSetLimit'
    { type' = pType_,
      delegationSetId = pDelegationSetId_
    }

-- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslType :: Lens.Lens' GetReusableDelegationSetLimit ReusableDelegationSetLimitType
grdslType = Lens.lens (type' :: GetReusableDelegationSetLimit -> ReusableDelegationSetLimitType) (\s a -> s {type' = a} :: GetReusableDelegationSetLimit)
{-# DEPRECATED grdslType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The ID of the delegation set that you want to get the limit for.
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslDelegationSetId :: Lens.Lens' GetReusableDelegationSetLimit ResourceId
grdslDelegationSetId = Lens.lens (delegationSetId :: GetReusableDelegationSetLimit -> ResourceId) (\s a -> s {delegationSetId = a} :: GetReusableDelegationSetLimit)
{-# DEPRECATED grdslDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

instance Lude.AWSRequest GetReusableDelegationSetLimit where
  type
    Rs GetReusableDelegationSetLimit =
      GetReusableDelegationSetLimitResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetReusableDelegationSetLimitResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "Limit")
            Lude.<*> (x Lude..@ "Count")
      )

instance Lude.ToHeaders GetReusableDelegationSetLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetReusableDelegationSetLimit where
  toPath GetReusableDelegationSetLimit' {..} =
    Lude.mconcat
      [ "/2013-04-01/reusabledelegationsetlimit/",
        Lude.toBS delegationSetId,
        "/",
        Lude.toBS type'
      ]

instance Lude.ToQuery GetReusableDelegationSetLimit where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'mkGetReusableDelegationSetLimitResponse' smart constructor.
data GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse'
  { responseStatus ::
      Lude.Int,
    limit ::
      ReusableDelegationSetLimit,
    count ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReusableDelegationSetLimitResponse' with the minimum fields required to make a request.
--
-- * 'count' - The current number of hosted zones that you can associate with the specified reusable delegation set.
-- * 'limit' - The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
-- * 'responseStatus' - The response status code.
mkGetReusableDelegationSetLimitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'limit'
  ReusableDelegationSetLimit ->
  -- | 'count'
  Lude.Natural ->
  GetReusableDelegationSetLimitResponse
mkGetReusableDelegationSetLimitResponse
  pResponseStatus_
  pLimit_
  pCount_ =
    GetReusableDelegationSetLimitResponse'
      { responseStatus =
          pResponseStatus_,
        limit = pLimit_,
        count = pCount_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrsResponseStatus :: Lens.Lens' GetReusableDelegationSetLimitResponse Lude.Int
grdslrsResponseStatus = Lens.lens (responseStatus :: GetReusableDelegationSetLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReusableDelegationSetLimitResponse)
{-# DEPRECATED grdslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrsLimit :: Lens.Lens' GetReusableDelegationSetLimitResponse ReusableDelegationSetLimit
grdslrsLimit = Lens.lens (limit :: GetReusableDelegationSetLimitResponse -> ReusableDelegationSetLimit) (\s a -> s {limit = a} :: GetReusableDelegationSetLimitResponse)
{-# DEPRECATED grdslrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrsCount :: Lens.Lens' GetReusableDelegationSetLimitResponse Lude.Natural
grdslrsCount = Lens.lens (count :: GetReusableDelegationSetLimitResponse -> Lude.Natural) (\s a -> s {count = a} :: GetReusableDelegationSetLimitResponse)
{-# DEPRECATED grdslrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

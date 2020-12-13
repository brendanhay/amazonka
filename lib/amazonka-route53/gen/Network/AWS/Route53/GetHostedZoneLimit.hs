{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZoneLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone.
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetHostedZoneLimit
  ( -- * Creating a request
    GetHostedZoneLimit (..),
    mkGetHostedZoneLimit,

    -- ** Request lenses
    ghzlHostedZoneId,
    ghzlType,

    -- * Destructuring the response
    GetHostedZoneLimitResponse (..),
    mkGetHostedZoneLimitResponse,

    -- ** Response lenses
    ghzlrsCount,
    ghzlrsLimit,
    ghzlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetHostedZoneLimit' smart constructor.
data GetHostedZoneLimit = GetHostedZoneLimit'
  { -- | The ID of the hosted zone that you want to get a limit for.
    hostedZoneId :: ResourceId,
    -- | The limit that you want to get. Valid values include the following:
    --
    --
    --     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
    --
    --
    --     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
    type' :: HostedZoneLimitType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZoneLimit' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that you want to get a limit for.
-- * 'type'' - The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
mkGetHostedZoneLimit ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'type''
  HostedZoneLimitType ->
  GetHostedZoneLimit
mkGetHostedZoneLimit pHostedZoneId_ pType_ =
  GetHostedZoneLimit'
    { hostedZoneId = pHostedZoneId_,
      type' = pType_
    }

-- | The ID of the hosted zone that you want to get a limit for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlHostedZoneId :: Lens.Lens' GetHostedZoneLimit ResourceId
ghzlHostedZoneId = Lens.lens (hostedZoneId :: GetHostedZoneLimit -> ResourceId) (\s a -> s {hostedZoneId = a} :: GetHostedZoneLimit)
{-# DEPRECATED ghzlHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlType :: Lens.Lens' GetHostedZoneLimit HostedZoneLimitType
ghzlType = Lens.lens (type' :: GetHostedZoneLimit -> HostedZoneLimitType) (\s a -> s {type' = a} :: GetHostedZoneLimit)
{-# DEPRECATED ghzlType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest GetHostedZoneLimit where
  type Rs GetHostedZoneLimit = GetHostedZoneLimitResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHostedZoneLimitResponse'
            Lude.<$> (x Lude..@ "Count")
            Lude.<*> (x Lude..@ "Limit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHostedZoneLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHostedZoneLimit where
  toPath GetHostedZoneLimit' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzonelimit/",
        Lude.toBS hostedZoneId,
        "/",
        Lude.toBS type'
      ]

instance Lude.ToQuery GetHostedZoneLimit where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'mkGetHostedZoneLimitResponse' smart constructor.
data GetHostedZoneLimitResponse = GetHostedZoneLimitResponse'
  { -- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
    count :: Lude.Natural,
    -- | The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
    limit :: HostedZoneLimit,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZoneLimitResponse' with the minimum fields required to make a request.
--
-- * 'count' - The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
-- * 'limit' - The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
-- * 'responseStatus' - The response status code.
mkGetHostedZoneLimitResponse ::
  -- | 'count'
  Lude.Natural ->
  -- | 'limit'
  HostedZoneLimit ->
  -- | 'responseStatus'
  Lude.Int ->
  GetHostedZoneLimitResponse
mkGetHostedZoneLimitResponse pCount_ pLimit_ pResponseStatus_ =
  GetHostedZoneLimitResponse'
    { count = pCount_,
      limit = pLimit_,
      responseStatus = pResponseStatus_
    }

-- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrsCount :: Lens.Lens' GetHostedZoneLimitResponse Lude.Natural
ghzlrsCount = Lens.lens (count :: GetHostedZoneLimitResponse -> Lude.Natural) (\s a -> s {count = a} :: GetHostedZoneLimitResponse)
{-# DEPRECATED ghzlrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrsLimit :: Lens.Lens' GetHostedZoneLimitResponse HostedZoneLimit
ghzlrsLimit = Lens.lens (limit :: GetHostedZoneLimitResponse -> HostedZoneLimit) (\s a -> s {limit = a} :: GetHostedZoneLimitResponse)
{-# DEPRECATED ghzlrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrsResponseStatus :: Lens.Lens' GetHostedZoneLimitResponse Lude.Int
ghzlrsResponseStatus = Lens.lens (responseStatus :: GetHostedZoneLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHostedZoneLimitResponse)
{-# DEPRECATED ghzlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

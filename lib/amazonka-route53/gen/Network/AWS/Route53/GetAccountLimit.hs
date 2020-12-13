{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetAccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for the current account, for example, the maximum number of health checks that you can create using the account.
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetAccountLimit
  ( -- * Creating a request
    GetAccountLimit (..),
    mkGetAccountLimit,

    -- ** Request lenses
    galType,

    -- * Destructuring the response
    GetAccountLimitResponse (..),
    mkGetAccountLimitResponse,

    -- ** Response lenses
    galrsCount,
    galrsLimit,
    galrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetAccountLimit' smart constructor.
newtype GetAccountLimit = GetAccountLimit'
  { -- | The limit that you want to get. Valid values include the following:
    --
    --
    --     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.
    --
    --
    --     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.
    --
    --
    --     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.
    --
    --
    --     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.
    --
    --
    --     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
    type' :: AccountLimitType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountLimit' with the minimum fields required to make a request.
--
-- * 'type'' - The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.
--
--
--     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.
--
--
--     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
mkGetAccountLimit ::
  -- | 'type''
  AccountLimitType ->
  GetAccountLimit
mkGetAccountLimit pType_ = GetAccountLimit' {type' = pType_}

-- | The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.
--
--
--     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.
--
--
--     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galType :: Lens.Lens' GetAccountLimit AccountLimitType
galType = Lens.lens (type' :: GetAccountLimit -> AccountLimitType) (\s a -> s {type' = a} :: GetAccountLimit)
{-# DEPRECATED galType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest GetAccountLimit where
  type Rs GetAccountLimit = GetAccountLimitResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetAccountLimitResponse'
            Lude.<$> (x Lude..@ "Count")
            Lude.<*> (x Lude..@ "Limit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountLimit where
  toPath GetAccountLimit' {..} =
    Lude.mconcat ["/2013-04-01/accountlimit/", Lude.toBS type']

instance Lude.ToQuery GetAccountLimit where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'mkGetAccountLimitResponse' smart constructor.
data GetAccountLimitResponse = GetAccountLimitResponse'
  { -- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Count@ is the current number of health checks that you have created using the current account.
    count :: Lude.Natural,
    -- | The current setting for the specified limit. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of health checks that you can create using the current account.
    limit :: AccountLimit,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountLimitResponse' with the minimum fields required to make a request.
--
-- * 'count' - The current number of entities that you have created of the specified type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Count@ is the current number of health checks that you have created using the current account.
-- * 'limit' - The current setting for the specified limit. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of health checks that you can create using the current account.
-- * 'responseStatus' - The response status code.
mkGetAccountLimitResponse ::
  -- | 'count'
  Lude.Natural ->
  -- | 'limit'
  AccountLimit ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountLimitResponse
mkGetAccountLimitResponse pCount_ pLimit_ pResponseStatus_ =
  GetAccountLimitResponse'
    { count = pCount_,
      limit = pLimit_,
      responseStatus = pResponseStatus_
    }

-- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Count@ is the current number of health checks that you have created using the current account.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsCount :: Lens.Lens' GetAccountLimitResponse Lude.Natural
galrsCount = Lens.lens (count :: GetAccountLimitResponse -> Lude.Natural) (\s a -> s {count = a} :: GetAccountLimitResponse)
{-# DEPRECATED galrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The current setting for the specified limit. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of health checks that you can create using the current account.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsLimit :: Lens.Lens' GetAccountLimitResponse AccountLimit
galrsLimit = Lens.lens (limit :: GetAccountLimitResponse -> AccountLimit) (\s a -> s {limit = a} :: GetAccountLimitResponse)
{-# DEPRECATED galrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsResponseStatus :: Lens.Lens' GetAccountLimitResponse Lude.Int
galrsResponseStatus = Lens.lens (responseStatus :: GetAccountLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountLimitResponse)
{-# DEPRECATED galrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

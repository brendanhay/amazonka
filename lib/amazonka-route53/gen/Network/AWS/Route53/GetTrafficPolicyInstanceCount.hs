{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstanceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the number of traffic policy instances that are associated with the current AWS account.
module Network.AWS.Route53.GetTrafficPolicyInstanceCount
  ( -- * Creating a request
    GetTrafficPolicyInstanceCount (..),
    mkGetTrafficPolicyInstanceCount,

    -- * Destructuring the response
    GetTrafficPolicyInstanceCountResponse (..),
    mkGetTrafficPolicyInstanceCountResponse,

    -- ** Response lenses
    gtpicrsResponseStatus,
    gtpicrsTrafficPolicyInstanceCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Request to get the number of traffic policy instances that are associated with the current AWS account.
--
-- /See:/ 'mkGetTrafficPolicyInstanceCount' smart constructor.
data GetTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrafficPolicyInstanceCount' with the minimum fields required to make a request.
mkGetTrafficPolicyInstanceCount ::
  GetTrafficPolicyInstanceCount
mkGetTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'

instance Lude.AWSRequest GetTrafficPolicyInstanceCount where
  type
    Rs GetTrafficPolicyInstanceCount =
      GetTrafficPolicyInstanceCountResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceCountResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "TrafficPolicyInstanceCount")
      )

instance Lude.ToHeaders GetTrafficPolicyInstanceCount where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTrafficPolicyInstanceCount where
  toPath = Lude.const "/2013-04-01/trafficpolicyinstancecount"

instance Lude.ToQuery GetTrafficPolicyInstanceCount where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkGetTrafficPolicyInstanceCountResponse' smart constructor.
data GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse'
  { responseStatus ::
      Lude.Int,
    trafficPolicyInstanceCount ::
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

-- | Creates a value of 'GetTrafficPolicyInstanceCountResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstanceCount' - The number of traffic policy instances that are associated with the current AWS account.
mkGetTrafficPolicyInstanceCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trafficPolicyInstanceCount'
  Lude.Int ->
  GetTrafficPolicyInstanceCountResponse
mkGetTrafficPolicyInstanceCountResponse
  pResponseStatus_
  pTrafficPolicyInstanceCount_ =
    GetTrafficPolicyInstanceCountResponse'
      { responseStatus =
          pResponseStatus_,
        trafficPolicyInstanceCount =
          pTrafficPolicyInstanceCount_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpicrsResponseStatus :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Lude.Int
gtpicrsResponseStatus = Lens.lens (responseStatus :: GetTrafficPolicyInstanceCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTrafficPolicyInstanceCountResponse)
{-# DEPRECATED gtpicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The number of traffic policy instances that are associated with the current AWS account.
--
-- /Note:/ Consider using 'trafficPolicyInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpicrsTrafficPolicyInstanceCount :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Lude.Int
gtpicrsTrafficPolicyInstanceCount = Lens.lens (trafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> Lude.Int) (\s a -> s {trafficPolicyInstanceCount = a} :: GetTrafficPolicyInstanceCountResponse)
{-# DEPRECATED gtpicrsTrafficPolicyInstanceCount "Use generic-lens or generic-optics with 'trafficPolicyInstanceCount' instead." #-}

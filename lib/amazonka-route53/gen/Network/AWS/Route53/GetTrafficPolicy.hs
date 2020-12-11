{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific traffic policy version.
--
-- For information about how of deleting a traffic policy affects the response from @GetTrafficPolicy@ , see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy> .
module Network.AWS.Route53.GetTrafficPolicy
  ( -- * Creating a request
    GetTrafficPolicy (..),
    mkGetTrafficPolicy,

    -- ** Request lenses
    gtpId,
    gtpVersion,

    -- * Destructuring the response
    GetTrafficPolicyResponse (..),
    mkGetTrafficPolicyResponse,

    -- ** Response lenses
    gtprsResponseStatus,
    gtprsTrafficPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Gets information about a specific traffic policy version.
--
-- /See:/ 'mkGetTrafficPolicy' smart constructor.
data GetTrafficPolicy = GetTrafficPolicy'
  { id :: Lude.Text,
    version :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrafficPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the traffic policy that you want to get information about.
-- * 'version' - The version number of the traffic policy that you want to get information about.
mkGetTrafficPolicy ::
  -- | 'id'
  Lude.Text ->
  -- | 'version'
  Lude.Natural ->
  GetTrafficPolicy
mkGetTrafficPolicy pId_ pVersion_ =
  GetTrafficPolicy' {id = pId_, version = pVersion_}

-- | The ID of the traffic policy that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpId :: Lens.Lens' GetTrafficPolicy Lude.Text
gtpId = Lens.lens (id :: GetTrafficPolicy -> Lude.Text) (\s a -> s {id = a} :: GetTrafficPolicy)
{-# DEPRECATED gtpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version number of the traffic policy that you want to get information about.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpVersion :: Lens.Lens' GetTrafficPolicy Lude.Natural
gtpVersion = Lens.lens (version :: GetTrafficPolicy -> Lude.Natural) (\s a -> s {version = a} :: GetTrafficPolicy)
{-# DEPRECATED gtpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetTrafficPolicy where
  type Rs GetTrafficPolicy = GetTrafficPolicyResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTrafficPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "TrafficPolicy")
      )

instance Lude.ToHeaders GetTrafficPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTrafficPolicy where
  toPath GetTrafficPolicy' {..} =
    Lude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Lude.toBS id,
        "/",
        Lude.toBS version
      ]

instance Lude.ToQuery GetTrafficPolicy where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkGetTrafficPolicyResponse' smart constructor.
data GetTrafficPolicyResponse = GetTrafficPolicyResponse'
  { responseStatus ::
      Lude.Int,
    trafficPolicy :: TrafficPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrafficPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicy' - A complex type that contains settings for the specified traffic policy.
mkGetTrafficPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  GetTrafficPolicyResponse
mkGetTrafficPolicyResponse pResponseStatus_ pTrafficPolicy_ =
  GetTrafficPolicyResponse'
    { responseStatus = pResponseStatus_,
      trafficPolicy = pTrafficPolicy_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtprsResponseStatus :: Lens.Lens' GetTrafficPolicyResponse Lude.Int
gtprsResponseStatus = Lens.lens (responseStatus :: GetTrafficPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTrafficPolicyResponse)
{-# DEPRECATED gtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains settings for the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtprsTrafficPolicy :: Lens.Lens' GetTrafficPolicyResponse TrafficPolicy
gtprsTrafficPolicy = Lens.lens (trafficPolicy :: GetTrafficPolicyResponse -> TrafficPolicy) (\s a -> s {trafficPolicy = a} :: GetTrafficPolicyResponse)
{-# DEPRECATED gtprsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

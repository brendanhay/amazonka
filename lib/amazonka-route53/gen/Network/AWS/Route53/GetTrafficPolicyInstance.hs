{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified traffic policy instance.
module Network.AWS.Route53.GetTrafficPolicyInstance
  ( -- * Creating a request
    GetTrafficPolicyInstance (..),
    mkGetTrafficPolicyInstance,

    -- ** Request lenses
    gtpiId,

    -- * Destructuring the response
    GetTrafficPolicyInstanceResponse (..),
    mkGetTrafficPolicyInstanceResponse,

    -- ** Response lenses
    gtpirsResponseStatus,
    gtpirsTrafficPolicyInstance,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Gets information about a specified traffic policy instance.
--
-- /See:/ 'mkGetTrafficPolicyInstance' smart constructor.
newtype GetTrafficPolicyInstance = GetTrafficPolicyInstance'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the traffic policy instance that you want to get information about.
mkGetTrafficPolicyInstance ::
  -- | 'id'
  Lude.Text ->
  GetTrafficPolicyInstance
mkGetTrafficPolicyInstance pId_ =
  GetTrafficPolicyInstance' {id = pId_}

-- | The ID of the traffic policy instance that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpiId :: Lens.Lens' GetTrafficPolicyInstance Lude.Text
gtpiId = Lens.lens (id :: GetTrafficPolicyInstance -> Lude.Text) (\s a -> s {id = a} :: GetTrafficPolicyInstance)
{-# DEPRECATED gtpiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetTrafficPolicyInstance where
  type Rs GetTrafficPolicyInstance = GetTrafficPolicyInstanceResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "TrafficPolicyInstance")
      )

instance Lude.ToHeaders GetTrafficPolicyInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTrafficPolicyInstance where
  toPath GetTrafficPolicyInstance' {..} =
    Lude.mconcat ["/2013-04-01/trafficpolicyinstance/", Lude.toBS id]

instance Lude.ToQuery GetTrafficPolicyInstance where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkGetTrafficPolicyInstanceResponse' smart constructor.
data GetTrafficPolicyInstanceResponse = GetTrafficPolicyInstanceResponse'
  { responseStatus ::
      Lude.Int,
    trafficPolicyInstance ::
      TrafficPolicyInstance
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstance' - A complex type that contains settings for the traffic policy instance.
mkGetTrafficPolicyInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trafficPolicyInstance'
  TrafficPolicyInstance ->
  GetTrafficPolicyInstanceResponse
mkGetTrafficPolicyInstanceResponse
  pResponseStatus_
  pTrafficPolicyInstance_ =
    GetTrafficPolicyInstanceResponse'
      { responseStatus =
          pResponseStatus_,
        trafficPolicyInstance = pTrafficPolicyInstance_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpirsResponseStatus :: Lens.Lens' GetTrafficPolicyInstanceResponse Lude.Int
gtpirsResponseStatus = Lens.lens (responseStatus :: GetTrafficPolicyInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTrafficPolicyInstanceResponse)
{-# DEPRECATED gtpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains settings for the traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpirsTrafficPolicyInstance :: Lens.Lens' GetTrafficPolicyInstanceResponse TrafficPolicyInstance
gtpirsTrafficPolicyInstance = Lens.lens (trafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TrafficPolicyInstance) (\s a -> s {trafficPolicyInstance = a} :: GetTrafficPolicyInstanceResponse)
{-# DEPRECATED gtpirsTrafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead." #-}

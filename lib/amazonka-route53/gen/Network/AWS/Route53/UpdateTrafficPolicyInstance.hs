{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.
--
-- When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Route 53 performs the following operations:
--
--     * Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets.
--
--
--     * When all of the new resource record sets have been created, Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.
--
--
--     * Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.
module Network.AWS.Route53.UpdateTrafficPolicyInstance
  ( -- * Creating a request
    UpdateTrafficPolicyInstance (..),
    mkUpdateTrafficPolicyInstance,

    -- ** Request lenses
    utpiId,
    utpiTTL,
    utpiTrafficPolicyId,
    utpiTrafficPolicyVersion,

    -- * Destructuring the response
    UpdateTrafficPolicyInstanceResponse (..),
    mkUpdateTrafficPolicyInstanceResponse,

    -- ** Response lenses
    utpirsResponseStatus,
    utpirsTrafficPolicyInstance,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.
--
-- /See:/ 'mkUpdateTrafficPolicyInstance' smart constructor.
data UpdateTrafficPolicyInstance = UpdateTrafficPolicyInstance'
  { id ::
      Lude.Text,
    tTL :: Lude.Natural,
    trafficPolicyId :: Lude.Text,
    trafficPolicyVersion ::
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

-- | Creates a value of 'UpdateTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the traffic policy instance that you want to update.
-- * 'tTL' - The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
-- * 'trafficPolicyId' - The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
-- * 'trafficPolicyVersion' - The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
mkUpdateTrafficPolicyInstance ::
  -- | 'id'
  Lude.Text ->
  -- | 'tTL'
  Lude.Natural ->
  -- | 'trafficPolicyId'
  Lude.Text ->
  -- | 'trafficPolicyVersion'
  Lude.Natural ->
  UpdateTrafficPolicyInstance
mkUpdateTrafficPolicyInstance
  pId_
  pTTL_
  pTrafficPolicyId_
  pTrafficPolicyVersion_ =
    UpdateTrafficPolicyInstance'
      { id = pId_,
        tTL = pTTL_,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_
      }

-- | The ID of the traffic policy instance that you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiId :: Lens.Lens' UpdateTrafficPolicyInstance Lude.Text
utpiId = Lens.lens (id :: UpdateTrafficPolicyInstance -> Lude.Text) (\s a -> s {id = a} :: UpdateTrafficPolicyInstance)
{-# DEPRECATED utpiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The TTL that you want Amazon Route 53 to assign to all of the updated resource record sets.
--
-- /Note:/ Consider using 'tTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTTL :: Lens.Lens' UpdateTrafficPolicyInstance Lude.Natural
utpiTTL = Lens.lens (tTL :: UpdateTrafficPolicyInstance -> Lude.Natural) (\s a -> s {tTL = a} :: UpdateTrafficPolicyInstance)
{-# DEPRECATED utpiTTL "Use generic-lens or generic-optics with 'tTL' instead." #-}

-- | The ID of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTrafficPolicyId :: Lens.Lens' UpdateTrafficPolicyInstance Lude.Text
utpiTrafficPolicyId = Lens.lens (trafficPolicyId :: UpdateTrafficPolicyInstance -> Lude.Text) (\s a -> s {trafficPolicyId = a} :: UpdateTrafficPolicyInstance)
{-# DEPRECATED utpiTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy that you want Amazon Route 53 to use to update resource record sets for the specified traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpiTrafficPolicyVersion :: Lens.Lens' UpdateTrafficPolicyInstance Lude.Natural
utpiTrafficPolicyVersion = Lens.lens (trafficPolicyVersion :: UpdateTrafficPolicyInstance -> Lude.Natural) (\s a -> s {trafficPolicyVersion = a} :: UpdateTrafficPolicyInstance)
{-# DEPRECATED utpiTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

instance Lude.AWSRequest UpdateTrafficPolicyInstance where
  type
    Rs UpdateTrafficPolicyInstance =
      UpdateTrafficPolicyInstanceResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateTrafficPolicyInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "TrafficPolicyInstance")
      )

instance Lude.ToElement UpdateTrafficPolicyInstance where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyInstanceRequest"

instance Lude.ToHeaders UpdateTrafficPolicyInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateTrafficPolicyInstance where
  toPath UpdateTrafficPolicyInstance' {..} =
    Lude.mconcat ["/2013-04-01/trafficpolicyinstance/", Lude.toBS id]

instance Lude.ToQuery UpdateTrafficPolicyInstance where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML UpdateTrafficPolicyInstance where
  toXML UpdateTrafficPolicyInstance' {..} =
    Lude.mconcat
      [ "TTL" Lude.@= tTL,
        "TrafficPolicyId" Lude.@= trafficPolicyId,
        "TrafficPolicyVersion" Lude.@= trafficPolicyVersion
      ]

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkUpdateTrafficPolicyInstanceResponse' smart constructor.
data UpdateTrafficPolicyInstanceResponse = UpdateTrafficPolicyInstanceResponse'
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

-- | Creates a value of 'UpdateTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstance' - A complex type that contains settings for the updated traffic policy instance.
mkUpdateTrafficPolicyInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trafficPolicyInstance'
  TrafficPolicyInstance ->
  UpdateTrafficPolicyInstanceResponse
mkUpdateTrafficPolicyInstanceResponse
  pResponseStatus_
  pTrafficPolicyInstance_ =
    UpdateTrafficPolicyInstanceResponse'
      { responseStatus =
          pResponseStatus_,
        trafficPolicyInstance = pTrafficPolicyInstance_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpirsResponseStatus :: Lens.Lens' UpdateTrafficPolicyInstanceResponse Lude.Int
utpirsResponseStatus = Lens.lens (responseStatus :: UpdateTrafficPolicyInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrafficPolicyInstanceResponse)
{-# DEPRECATED utpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains settings for the updated traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpirsTrafficPolicyInstance :: Lens.Lens' UpdateTrafficPolicyInstanceResponse TrafficPolicyInstance
utpirsTrafficPolicyInstance = Lens.lens (trafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TrafficPolicyInstance) (\s a -> s {trafficPolicyInstance = a} :: UpdateTrafficPolicyInstanceResponse)
{-# DEPRECATED utpirsTrafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead." #-}

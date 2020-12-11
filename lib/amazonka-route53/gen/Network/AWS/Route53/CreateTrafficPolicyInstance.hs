{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, @CreateTrafficPolicyInstance@ associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that @CreateTrafficPolicyInstance@ created.
module Network.AWS.Route53.CreateTrafficPolicyInstance
  ( -- * Creating a request
    CreateTrafficPolicyInstance (..),
    mkCreateTrafficPolicyInstance,

    -- ** Request lenses
    ctpiHostedZoneId,
    ctpiName,
    ctpiTTL,
    ctpiTrafficPolicyId,
    ctpiTrafficPolicyVersion,

    -- * Destructuring the response
    CreateTrafficPolicyInstanceResponse (..),
    mkCreateTrafficPolicyInstanceResponse,

    -- ** Response lenses
    ctpirsResponseStatus,
    ctpirsTrafficPolicyInstance,
    ctpirsLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.
--
-- /See:/ 'mkCreateTrafficPolicyInstance' smart constructor.
data CreateTrafficPolicyInstance = CreateTrafficPolicyInstance'
  { hostedZoneId ::
      ResourceId,
    name :: Lude.Text,
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

-- | Creates a value of 'CreateTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to create resource record sets in by using the configuration in a traffic policy.
-- * 'name' - The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Route 53 creates for this traffic policy instance.
-- * 'tTL' - (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
-- * 'trafficPolicyId' - The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
-- * 'trafficPolicyVersion' - The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
mkCreateTrafficPolicyInstance ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Lude.Text ->
  -- | 'tTL'
  Lude.Natural ->
  -- | 'trafficPolicyId'
  Lude.Text ->
  -- | 'trafficPolicyVersion'
  Lude.Natural ->
  CreateTrafficPolicyInstance
mkCreateTrafficPolicyInstance
  pHostedZoneId_
  pName_
  pTTL_
  pTrafficPolicyId_
  pTrafficPolicyVersion_ =
    CreateTrafficPolicyInstance'
      { hostedZoneId = pHostedZoneId_,
        name = pName_,
        tTL = pTTL_,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_
      }

-- | The ID of the hosted zone that you want Amazon Route 53 to create resource record sets in by using the configuration in a traffic policy.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiHostedZoneId :: Lens.Lens' CreateTrafficPolicyInstance ResourceId
ctpiHostedZoneId = Lens.lens (hostedZoneId :: CreateTrafficPolicyInstance -> ResourceId) (\s a -> s {hostedZoneId = a} :: CreateTrafficPolicyInstance)
{-# DEPRECATED ctpiHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The domain name (such as example.com) or subdomain name (such as www.example.com) for which Amazon Route 53 responds to DNS queries by using the resource record sets that Route 53 creates for this traffic policy instance.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiName :: Lens.Lens' CreateTrafficPolicyInstance Lude.Text
ctpiName = Lens.lens (name :: CreateTrafficPolicyInstance -> Lude.Text) (\s a -> s {name = a} :: CreateTrafficPolicyInstance)
{-# DEPRECATED ctpiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | (Optional) The TTL that you want Amazon Route 53 to assign to all of the resource record sets that it creates in the specified hosted zone.
--
-- /Note:/ Consider using 'tTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTTL :: Lens.Lens' CreateTrafficPolicyInstance Lude.Natural
ctpiTTL = Lens.lens (tTL :: CreateTrafficPolicyInstance -> Lude.Natural) (\s a -> s {tTL = a} :: CreateTrafficPolicyInstance)
{-# DEPRECATED ctpiTTL "Use generic-lens or generic-optics with 'tTL' instead." #-}

-- | The ID of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTrafficPolicyId :: Lens.Lens' CreateTrafficPolicyInstance Lude.Text
ctpiTrafficPolicyId = Lens.lens (trafficPolicyId :: CreateTrafficPolicyInstance -> Lude.Text) (\s a -> s {trafficPolicyId = a} :: CreateTrafficPolicyInstance)
{-# DEPRECATED ctpiTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy that you want to use to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpiTrafficPolicyVersion :: Lens.Lens' CreateTrafficPolicyInstance Lude.Natural
ctpiTrafficPolicyVersion = Lens.lens (trafficPolicyVersion :: CreateTrafficPolicyInstance -> Lude.Natural) (\s a -> s {trafficPolicyVersion = a} :: CreateTrafficPolicyInstance)
{-# DEPRECATED ctpiTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

instance Lude.AWSRequest CreateTrafficPolicyInstance where
  type
    Rs CreateTrafficPolicyInstance =
      CreateTrafficPolicyInstanceResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficPolicyInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "TrafficPolicyInstance")
            Lude.<*> (h Lude..# "Location")
      )

instance Lude.ToElement CreateTrafficPolicyInstance where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyInstanceRequest"

instance Lude.ToHeaders CreateTrafficPolicyInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficPolicyInstance where
  toPath = Lude.const "/2013-04-01/trafficpolicyinstance"

instance Lude.ToQuery CreateTrafficPolicyInstance where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateTrafficPolicyInstance where
  toXML CreateTrafficPolicyInstance' {..} =
    Lude.mconcat
      [ "HostedZoneId" Lude.@= hostedZoneId,
        "Name" Lude.@= name,
        "TTL" Lude.@= tTL,
        "TrafficPolicyId" Lude.@= trafficPolicyId,
        "TrafficPolicyVersion" Lude.@= trafficPolicyVersion
      ]

-- | A complex type that contains the response information for the @CreateTrafficPolicyInstance@ request.
--
-- /See:/ 'mkCreateTrafficPolicyInstanceResponse' smart constructor.
data CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse'
  { responseStatus ::
      Lude.Int,
    trafficPolicyInstance ::
      TrafficPolicyInstance,
    location ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- * 'location' - A unique URL that represents a new traffic policy instance.
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstance' - A complex type that contains settings for the new traffic policy instance.
mkCreateTrafficPolicyInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trafficPolicyInstance'
  TrafficPolicyInstance ->
  -- | 'location'
  Lude.Text ->
  CreateTrafficPolicyInstanceResponse
mkCreateTrafficPolicyInstanceResponse
  pResponseStatus_
  pTrafficPolicyInstance_
  pLocation_ =
    CreateTrafficPolicyInstanceResponse'
      { responseStatus =
          pResponseStatus_,
        trafficPolicyInstance = pTrafficPolicyInstance_,
        location = pLocation_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirsResponseStatus :: Lens.Lens' CreateTrafficPolicyInstanceResponse Lude.Int
ctpirsResponseStatus = Lens.lens (responseStatus :: CreateTrafficPolicyInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficPolicyInstanceResponse)
{-# DEPRECATED ctpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains settings for the new traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirsTrafficPolicyInstance :: Lens.Lens' CreateTrafficPolicyInstanceResponse TrafficPolicyInstance
ctpirsTrafficPolicyInstance = Lens.lens (trafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TrafficPolicyInstance) (\s a -> s {trafficPolicyInstance = a} :: CreateTrafficPolicyInstanceResponse)
{-# DEPRECATED ctpirsTrafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead." #-}

-- | A unique URL that represents a new traffic policy instance.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpirsLocation :: Lens.Lens' CreateTrafficPolicyInstanceResponse Lude.Text
ctpirsLocation = Lens.lens (location :: CreateTrafficPolicyInstanceResponse -> Lude.Text) (\s a -> s {location = a} :: CreateTrafficPolicyInstanceResponse)
{-# DEPRECATED ctpirsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

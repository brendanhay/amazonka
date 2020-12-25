{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicyInstance
  ( TrafficPolicyInstance (..),

    -- * Smart constructor
    mkTrafficPolicyInstance,

    -- * Lenses
    tpiId,
    tpiHostedZoneId,
    tpiName,
    tpiTTL,
    tpiState,
    tpiMessage,
    tpiTrafficPolicyId,
    tpiTrafficPolicyVersion,
    tpiTrafficPolicyType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Message as Types
import qualified Network.AWS.Route53.Types.Name as Types
import qualified Network.AWS.Route53.Types.RecordType as Types
import qualified Network.AWS.Route53.Types.State as Types
import qualified Network.AWS.Route53.Types.TrafficPolicyId as Types
import qualified Network.AWS.Route53.Types.TrafficPolicyInstanceId as Types

-- | A complex type that contains settings for the new traffic policy instance.
--
-- /See:/ 'mkTrafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
  { -- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
    id :: Types.TrafficPolicyInstanceId,
    -- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
    hostedZoneId :: Types.ResourceId,
    -- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
    name :: Types.Name,
    -- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
    ttl :: Core.Natural,
    -- | The value of @State@ is one of the following values:
    --
    --
    --     * Applied
    --
    --     * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.
    --
    --
    --     * Creating
    --
    --     * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.
    --
    --
    --     * Failed
    --
    --     * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
    state :: Types.State,
    -- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
    message :: Types.Message,
    -- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
    trafficPolicyId :: Types.TrafficPolicyId,
    -- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
    trafficPolicyVersion :: Core.Natural,
    -- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
    trafficPolicyType :: Types.RecordType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficPolicyInstance' value with any optional fields omitted.
mkTrafficPolicyInstance ::
  -- | 'id'
  Types.TrafficPolicyInstanceId ->
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'name'
  Types.Name ->
  -- | 'ttl'
  Core.Natural ->
  -- | 'state'
  Types.State ->
  -- | 'message'
  Types.Message ->
  -- | 'trafficPolicyId'
  Types.TrafficPolicyId ->
  -- | 'trafficPolicyVersion'
  Core.Natural ->
  -- | 'trafficPolicyType'
  Types.RecordType ->
  TrafficPolicyInstance
mkTrafficPolicyInstance
  id
  hostedZoneId
  name
  ttl
  state
  message
  trafficPolicyId
  trafficPolicyVersion
  trafficPolicyType =
    TrafficPolicyInstance'
      { id,
        hostedZoneId,
        name,
        ttl,
        state,
        message,
        trafficPolicyId,
        trafficPolicyVersion,
        trafficPolicyType
      }

-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiId :: Lens.Lens' TrafficPolicyInstance Types.TrafficPolicyInstanceId
tpiId = Lens.field @"id"
{-# DEPRECATED tpiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiHostedZoneId :: Lens.Lens' TrafficPolicyInstance Types.ResourceId
tpiHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED tpiHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiName :: Lens.Lens' TrafficPolicyInstance Types.Name
tpiName = Lens.field @"name"
{-# DEPRECATED tpiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTTL :: Lens.Lens' TrafficPolicyInstance Core.Natural
tpiTTL = Lens.field @"ttl"
{-# DEPRECATED tpiTTL "Use generic-lens or generic-optics with 'ttl' instead." #-}

-- | The value of @State@ is one of the following values:
--
--
--     * Applied
--
--     * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.
--
--
--     * Creating
--
--     * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.
--
--
--     * Failed
--
--     * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiState :: Lens.Lens' TrafficPolicyInstance Types.State
tpiState = Lens.field @"state"
{-# DEPRECATED tpiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiMessage :: Lens.Lens' TrafficPolicyInstance Types.Message
tpiMessage = Lens.field @"message"
{-# DEPRECATED tpiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyId :: Lens.Lens' TrafficPolicyInstance Types.TrafficPolicyId
tpiTrafficPolicyId = Lens.field @"trafficPolicyId"
{-# DEPRECATED tpiTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyVersion :: Lens.Lens' TrafficPolicyInstance Core.Natural
tpiTrafficPolicyVersion = Lens.field @"trafficPolicyVersion"
{-# DEPRECATED tpiTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

-- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyType :: Lens.Lens' TrafficPolicyInstance Types.RecordType
tpiTrafficPolicyType = Lens.field @"trafficPolicyType"
{-# DEPRECATED tpiTrafficPolicyType "Use generic-lens or generic-optics with 'trafficPolicyType' instead." #-}

instance Core.FromXML TrafficPolicyInstance where
  parseXML x =
    TrafficPolicyInstance'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "HostedZoneId")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "TTL")
      Core.<*> (x Core..@ "State")
      Core.<*> (x Core..@ "Message")
      Core.<*> (x Core..@ "TrafficPolicyId")
      Core.<*> (x Core..@ "TrafficPolicyVersion")
      Core.<*> (x Core..@ "TrafficPolicyType")

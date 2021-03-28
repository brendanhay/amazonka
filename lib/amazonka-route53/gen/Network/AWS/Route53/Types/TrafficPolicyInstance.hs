{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.TrafficPolicyInstance
  ( TrafficPolicyInstance (..)
  -- * Smart constructor
  , mkTrafficPolicyInstance
  -- * Lenses
  , tpiId
  , tpiHostedZoneId
  , tpiName
  , tpiTTL
  , tpiState
  , tpiMessage
  , tpiTrafficPolicyId
  , tpiTrafficPolicyVersion
  , tpiTrafficPolicyType
  ) where

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
  { id :: Types.TrafficPolicyInstanceId
    -- ^ The ID that Amazon Route 53 assigned to the new traffic policy instance.
  , hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that Amazon Route 53 created resource record sets in.
  , name :: Types.Name
    -- ^ The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance. 
  , ttl :: Core.Natural
    -- ^ The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
  , state :: Types.State
    -- ^ The value of @State@ is one of the following values:
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
  , message :: Types.Message
    -- ^ If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
  , trafficPolicyId :: Types.TrafficPolicyId
    -- ^ The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
  , trafficPolicyVersion :: Core.Natural
    -- ^ The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
  , trafficPolicyType :: Types.RecordType
    -- ^ The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficPolicyInstance' value with any optional fields omitted.
mkTrafficPolicyInstance
    :: Types.TrafficPolicyInstanceId -- ^ 'id'
    -> Types.ResourceId -- ^ 'hostedZoneId'
    -> Types.Name -- ^ 'name'
    -> Core.Natural -- ^ 'ttl'
    -> Types.State -- ^ 'state'
    -> Types.Message -- ^ 'message'
    -> Types.TrafficPolicyId -- ^ 'trafficPolicyId'
    -> Core.Natural -- ^ 'trafficPolicyVersion'
    -> Types.RecordType -- ^ 'trafficPolicyType'
    -> TrafficPolicyInstance
mkTrafficPolicyInstance id hostedZoneId name ttl state message
  trafficPolicyId trafficPolicyVersion trafficPolicyType
  = TrafficPolicyInstance'{id, hostedZoneId, name, ttl, state,
                           message, trafficPolicyId, trafficPolicyVersion, trafficPolicyType}

-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiId :: Lens.Lens' TrafficPolicyInstance Types.TrafficPolicyInstanceId
tpiId = Lens.field @"id"
{-# INLINEABLE tpiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiHostedZoneId :: Lens.Lens' TrafficPolicyInstance Types.ResourceId
tpiHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE tpiHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiName :: Lens.Lens' TrafficPolicyInstance Types.Name
tpiName = Lens.field @"name"
{-# INLINEABLE tpiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTTL :: Lens.Lens' TrafficPolicyInstance Core.Natural
tpiTTL = Lens.field @"ttl"
{-# INLINEABLE tpiTTL #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

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
{-# INLINEABLE tpiState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiMessage :: Lens.Lens' TrafficPolicyInstance Types.Message
tpiMessage = Lens.field @"message"
{-# INLINEABLE tpiMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyId :: Lens.Lens' TrafficPolicyInstance Types.TrafficPolicyId
tpiTrafficPolicyId = Lens.field @"trafficPolicyId"
{-# INLINEABLE tpiTrafficPolicyId #-}
{-# DEPRECATED trafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead"  #-}

-- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyVersion :: Lens.Lens' TrafficPolicyInstance Core.Natural
tpiTrafficPolicyVersion = Lens.field @"trafficPolicyVersion"
{-# INLINEABLE tpiTrafficPolicyVersion #-}
{-# DEPRECATED trafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead"  #-}

-- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance. 
--
-- /Note:/ Consider using 'trafficPolicyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyType :: Lens.Lens' TrafficPolicyInstance Types.RecordType
tpiTrafficPolicyType = Lens.field @"trafficPolicyType"
{-# INLINEABLE tpiTrafficPolicyType #-}
{-# DEPRECATED trafficPolicyType "Use generic-lens or generic-optics with 'trafficPolicyType' instead"  #-}

instance Core.FromXML TrafficPolicyInstance where
        parseXML x
          = TrafficPolicyInstance' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "HostedZoneId" Core.<*>
                x Core..@ "Name"
                Core.<*> x Core..@ "TTL"
                Core.<*> x Core..@ "State"
                Core.<*> x Core..@ "Message"
                Core.<*> x Core..@ "TrafficPolicyId"
                Core.<*> x Core..@ "TrafficPolicyVersion"
                Core.<*> x Core..@ "TrafficPolicyType"

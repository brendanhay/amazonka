{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.TrafficPolicySummary
  ( TrafficPolicySummary (..)
  -- * Smart constructor
  , mkTrafficPolicySummary
  -- * Lenses
  , tpsId
  , tpsName
  , tpsType
  , tpsLatestVersion
  , tpsTrafficPolicyCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.RecordType as Types
import qualified Network.AWS.Route53.Types.TrafficPolicyId as Types
import qualified Network.AWS.Route53.Types.TrafficPolicyName as Types

-- | A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.
--
-- /See:/ 'mkTrafficPolicySummary' smart constructor.
data TrafficPolicySummary = TrafficPolicySummary'
  { id :: Types.TrafficPolicyId
    -- ^ The ID that Amazon Route 53 assigned to the traffic policy when you created it.
  , name :: Types.TrafficPolicyName
    -- ^ The name that you specified for the traffic policy when you created it.
  , type' :: Types.RecordType
    -- ^ The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
  , latestVersion :: Core.Natural
    -- ^ The version number of the latest version of the traffic policy.
  , trafficPolicyCount :: Core.Natural
    -- ^ The number of traffic policies that are associated with the current AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficPolicySummary' value with any optional fields omitted.
mkTrafficPolicySummary
    :: Types.TrafficPolicyId -- ^ 'id'
    -> Types.TrafficPolicyName -- ^ 'name'
    -> Types.RecordType -- ^ 'type\''
    -> Core.Natural -- ^ 'latestVersion'
    -> Core.Natural -- ^ 'trafficPolicyCount'
    -> TrafficPolicySummary
mkTrafficPolicySummary id name type' latestVersion
  trafficPolicyCount
  = TrafficPolicySummary'{id, name, type', latestVersion,
                          trafficPolicyCount}

-- | The ID that Amazon Route 53 assigned to the traffic policy when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpsId :: Lens.Lens' TrafficPolicySummary Types.TrafficPolicyId
tpsId = Lens.field @"id"
{-# INLINEABLE tpsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name that you specified for the traffic policy when you created it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpsName :: Lens.Lens' TrafficPolicySummary Types.TrafficPolicyName
tpsName = Lens.field @"name"
{-# INLINEABLE tpsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpsType :: Lens.Lens' TrafficPolicySummary Types.RecordType
tpsType = Lens.field @"type'"
{-# INLINEABLE tpsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The version number of the latest version of the traffic policy.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpsLatestVersion :: Lens.Lens' TrafficPolicySummary Core.Natural
tpsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE tpsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The number of traffic policies that are associated with the current AWS account.
--
-- /Note:/ Consider using 'trafficPolicyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpsTrafficPolicyCount :: Lens.Lens' TrafficPolicySummary Core.Natural
tpsTrafficPolicyCount = Lens.field @"trafficPolicyCount"
{-# INLINEABLE tpsTrafficPolicyCount #-}
{-# DEPRECATED trafficPolicyCount "Use generic-lens or generic-optics with 'trafficPolicyCount' instead"  #-}

instance Core.FromXML TrafficPolicySummary where
        parseXML x
          = TrafficPolicySummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "Name" Core.<*>
                x Core..@ "Type"
                Core.<*> x Core..@ "LatestVersion"
                Core.<*> x Core..@ "TrafficPolicyCount"

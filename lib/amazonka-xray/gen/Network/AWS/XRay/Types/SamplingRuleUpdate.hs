{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.SamplingRuleUpdate
  ( SamplingRuleUpdate (..)
  -- * Smart constructor
  , mkSamplingRuleUpdate
  -- * Lenses
  , sruAttributes
  , sruFixedRate
  , sruHTTPMethod
  , sruHost
  , sruPriority
  , sruReservoirSize
  , sruResourceARN
  , sruRuleARN
  , sruRuleName
  , sruServiceName
  , sruServiceType
  , sruURLPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AttributeKey as Types
import qualified Network.AWS.XRay.Types.AttributeValue as Types
import qualified Network.AWS.XRay.Types.HTTPMethod as Types
import qualified Network.AWS.XRay.Types.Host as Types
import qualified Network.AWS.XRay.Types.ResourceARN as Types
import qualified Network.AWS.XRay.Types.RuleName as Types
import qualified Network.AWS.XRay.Types.ServiceName as Types
import qualified Network.AWS.XRay.Types.ServiceType as Types
import qualified Network.AWS.XRay.Types.URLPath as Types

-- | A document specifying changes to a sampling rule's configuration.
--
-- /See:/ 'mkSamplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue)
    -- ^ Matches attributes derived from the request.
  , fixedRate :: Core.Maybe Core.Double
    -- ^ The percentage of matching requests to instrument, after the reservoir is exhausted.
  , hTTPMethod :: Core.Maybe Types.HTTPMethod
    -- ^ Matches the HTTP method of a request.
  , host :: Core.Maybe Types.Host
    -- ^ Matches the hostname from a request URL.
  , priority :: Core.Maybe Core.Int
    -- ^ The priority of the sampling rule.
  , reservoirSize :: Core.Maybe Core.Int
    -- ^ A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
  , resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ Matches the ARN of the AWS resource on which the service runs.
  , ruleARN :: Core.Maybe Core.Text
    -- ^ The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
  , ruleName :: Core.Maybe Types.RuleName
    -- ^ The name of the sampling rule. Specify a rule by either name or ARN, but not both.
  , serviceName :: Core.Maybe Types.ServiceName
    -- ^ Matches the @name@ that the service uses to identify itself in segments.
  , serviceType :: Core.Maybe Types.ServiceType
    -- ^ Matches the @origin@ that the service uses to identify its type in segments.
  , uRLPath :: Core.Maybe Types.URLPath
    -- ^ Matches the path from a request URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SamplingRuleUpdate' value with any optional fields omitted.
mkSamplingRuleUpdate
    :: SamplingRuleUpdate
mkSamplingRuleUpdate
  = SamplingRuleUpdate'{attributes = Core.Nothing,
                        fixedRate = Core.Nothing, hTTPMethod = Core.Nothing,
                        host = Core.Nothing, priority = Core.Nothing,
                        reservoirSize = Core.Nothing, resourceARN = Core.Nothing,
                        ruleARN = Core.Nothing, ruleName = Core.Nothing,
                        serviceName = Core.Nothing, serviceType = Core.Nothing,
                        uRLPath = Core.Nothing}

-- | Matches attributes derived from the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruAttributes :: Lens.Lens' SamplingRuleUpdate (Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue))
sruAttributes = Lens.field @"attributes"
{-# INLINEABLE sruAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruFixedRate :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Double)
sruFixedRate = Lens.field @"fixedRate"
{-# INLINEABLE sruFixedRate #-}
{-# DEPRECATED fixedRate "Use generic-lens or generic-optics with 'fixedRate' instead"  #-}

-- | Matches the HTTP method of a request.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruHTTPMethod :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.HTTPMethod)
sruHTTPMethod = Lens.field @"hTTPMethod"
{-# INLINEABLE sruHTTPMethod #-}
{-# DEPRECATED hTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead"  #-}

-- | Matches the hostname from a request URL.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruHost :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.Host)
sruHost = Lens.field @"host"
{-# INLINEABLE sruHost #-}
{-# DEPRECATED host "Use generic-lens or generic-optics with 'host' instead"  #-}

-- | The priority of the sampling rule.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruPriority :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Int)
sruPriority = Lens.field @"priority"
{-# INLINEABLE sruPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- /Note:/ Consider using 'reservoirSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruReservoirSize :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Int)
sruReservoirSize = Lens.field @"reservoirSize"
{-# INLINEABLE sruReservoirSize #-}
{-# DEPRECATED reservoirSize "Use generic-lens or generic-optics with 'reservoirSize' instead"  #-}

-- | Matches the ARN of the AWS resource on which the service runs.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruResourceARN :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.ResourceARN)
sruResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE sruResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruRuleARN :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
sruRuleARN = Lens.field @"ruleARN"
{-# INLINEABLE sruRuleARN #-}
{-# DEPRECATED ruleARN "Use generic-lens or generic-optics with 'ruleARN' instead"  #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruRuleName :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.RuleName)
sruRuleName = Lens.field @"ruleName"
{-# INLINEABLE sruRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | Matches the @name@ that the service uses to identify itself in segments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruServiceName :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.ServiceName)
sruServiceName = Lens.field @"serviceName"
{-# INLINEABLE sruServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | Matches the @origin@ that the service uses to identify its type in segments.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruServiceType :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.ServiceType)
sruServiceType = Lens.field @"serviceType"
{-# INLINEABLE sruServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

-- | Matches the path from a request URL.
--
-- /Note:/ Consider using 'uRLPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruURLPath :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Types.URLPath)
sruURLPath = Lens.field @"uRLPath"
{-# INLINEABLE sruURLPath #-}
{-# DEPRECATED uRLPath "Use generic-lens or generic-optics with 'uRLPath' instead"  #-}

instance Core.FromJSON SamplingRuleUpdate where
        toJSON SamplingRuleUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("Attributes" Core..=) Core.<$> attributes,
                  ("FixedRate" Core..=) Core.<$> fixedRate,
                  ("HTTPMethod" Core..=) Core.<$> hTTPMethod,
                  ("Host" Core..=) Core.<$> host,
                  ("Priority" Core..=) Core.<$> priority,
                  ("ReservoirSize" Core..=) Core.<$> reservoirSize,
                  ("ResourceARN" Core..=) Core.<$> resourceARN,
                  ("RuleARN" Core..=) Core.<$> ruleARN,
                  ("RuleName" Core..=) Core.<$> ruleName,
                  ("ServiceName" Core..=) Core.<$> serviceName,
                  ("ServiceType" Core..=) Core.<$> serviceType,
                  ("URLPath" Core..=) Core.<$> uRLPath])

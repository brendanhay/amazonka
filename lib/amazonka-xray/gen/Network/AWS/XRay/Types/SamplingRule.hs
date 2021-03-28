{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.SamplingRule
  ( SamplingRule (..)
  -- * Smart constructor
  , mkSamplingRule
  -- * Lenses
  , srResourceARN
  , srPriority
  , srFixedRate
  , srReservoirSize
  , srServiceName
  , srServiceType
  , srHost
  , srHTTPMethod
  , srURLPath
  , srVersion
  , srAttributes
  , srRuleARN
  , srRuleName
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

-- | A sampling rule that services use to decide whether to instrument a request. Rule fields can match properties of the service, or properties of a request. The service can ignore rules that don't match its properties.
--
-- /See:/ 'mkSamplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { resourceARN :: Types.ResourceARN
    -- ^ Matches the ARN of the AWS resource on which the service runs.
  , priority :: Core.Natural
    -- ^ The priority of the sampling rule.
  , fixedRate :: Core.Double
    -- ^ The percentage of matching requests to instrument, after the reservoir is exhausted.
  , reservoirSize :: Core.Natural
    -- ^ A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
  , serviceName :: Types.ServiceName
    -- ^ Matches the @name@ that the service uses to identify itself in segments.
  , serviceType :: Types.ServiceType
    -- ^ Matches the @origin@ that the service uses to identify its type in segments.
  , host :: Types.Host
    -- ^ Matches the hostname from a request URL.
  , hTTPMethod :: Types.HTTPMethod
    -- ^ Matches the HTTP method of a request.
  , uRLPath :: Types.URLPath
    -- ^ Matches the path from a request URL.
  , version :: Core.Natural
    -- ^ The version of the sampling rule format (@1@ ).
  , attributes :: Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue)
    -- ^ Matches attributes derived from the request.
  , ruleARN :: Core.Maybe Core.Text
    -- ^ The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
  , ruleName :: Core.Maybe Types.RuleName
    -- ^ The name of the sampling rule. Specify a rule by either name or ARN, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SamplingRule' value with any optional fields omitted.
mkSamplingRule
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> Core.Natural -- ^ 'priority'
    -> Core.Double -- ^ 'fixedRate'
    -> Core.Natural -- ^ 'reservoirSize'
    -> Types.ServiceName -- ^ 'serviceName'
    -> Types.ServiceType -- ^ 'serviceType'
    -> Types.Host -- ^ 'host'
    -> Types.HTTPMethod -- ^ 'hTTPMethod'
    -> Types.URLPath -- ^ 'uRLPath'
    -> Core.Natural -- ^ 'version'
    -> SamplingRule
mkSamplingRule resourceARN priority fixedRate reservoirSize
  serviceName serviceType host hTTPMethod uRLPath version
  = SamplingRule'{resourceARN, priority, fixedRate, reservoirSize,
                  serviceName, serviceType, host, hTTPMethod, uRLPath, version,
                  attributes = Core.Nothing, ruleARN = Core.Nothing,
                  ruleName = Core.Nothing}

-- | Matches the ARN of the AWS resource on which the service runs.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceARN :: Lens.Lens' SamplingRule Types.ResourceARN
srResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE srResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The priority of the sampling rule.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPriority :: Lens.Lens' SamplingRule Core.Natural
srPriority = Lens.field @"priority"
{-# INLINEABLE srPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srFixedRate :: Lens.Lens' SamplingRule Core.Double
srFixedRate = Lens.field @"fixedRate"
{-# INLINEABLE srFixedRate #-}
{-# DEPRECATED fixedRate "Use generic-lens or generic-optics with 'fixedRate' instead"  #-}

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- /Note:/ Consider using 'reservoirSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srReservoirSize :: Lens.Lens' SamplingRule Core.Natural
srReservoirSize = Lens.field @"reservoirSize"
{-# INLINEABLE srReservoirSize #-}
{-# DEPRECATED reservoirSize "Use generic-lens or generic-optics with 'reservoirSize' instead"  #-}

-- | Matches the @name@ that the service uses to identify itself in segments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceName :: Lens.Lens' SamplingRule Types.ServiceName
srServiceName = Lens.field @"serviceName"
{-# INLINEABLE srServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | Matches the @origin@ that the service uses to identify its type in segments.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceType :: Lens.Lens' SamplingRule Types.ServiceType
srServiceType = Lens.field @"serviceType"
{-# INLINEABLE srServiceType #-}
{-# DEPRECATED serviceType "Use generic-lens or generic-optics with 'serviceType' instead"  #-}

-- | Matches the hostname from a request URL.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHost :: Lens.Lens' SamplingRule Types.Host
srHost = Lens.field @"host"
{-# INLINEABLE srHost #-}
{-# DEPRECATED host "Use generic-lens or generic-optics with 'host' instead"  #-}

-- | Matches the HTTP method of a request.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHTTPMethod :: Lens.Lens' SamplingRule Types.HTTPMethod
srHTTPMethod = Lens.field @"hTTPMethod"
{-# INLINEABLE srHTTPMethod #-}
{-# DEPRECATED hTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead"  #-}

-- | Matches the path from a request URL.
--
-- /Note:/ Consider using 'uRLPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srURLPath :: Lens.Lens' SamplingRule Types.URLPath
srURLPath = Lens.field @"uRLPath"
{-# INLINEABLE srURLPath #-}
{-# DEPRECATED uRLPath "Use generic-lens or generic-optics with 'uRLPath' instead"  #-}

-- | The version of the sampling rule format (@1@ ).
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srVersion :: Lens.Lens' SamplingRule Core.Natural
srVersion = Lens.field @"version"
{-# INLINEABLE srVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | Matches attributes derived from the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAttributes :: Lens.Lens' SamplingRule (Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue))
srAttributes = Lens.field @"attributes"
{-# INLINEABLE srAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleARN :: Lens.Lens' SamplingRule (Core.Maybe Core.Text)
srRuleARN = Lens.field @"ruleARN"
{-# INLINEABLE srRuleARN #-}
{-# DEPRECATED ruleARN "Use generic-lens or generic-optics with 'ruleARN' instead"  #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleName :: Lens.Lens' SamplingRule (Core.Maybe Types.RuleName)
srRuleName = Lens.field @"ruleName"
{-# INLINEABLE srRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.FromJSON SamplingRule where
        toJSON SamplingRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("Priority" Core..= priority),
                  Core.Just ("FixedRate" Core..= fixedRate),
                  Core.Just ("ReservoirSize" Core..= reservoirSize),
                  Core.Just ("ServiceName" Core..= serviceName),
                  Core.Just ("ServiceType" Core..= serviceType),
                  Core.Just ("Host" Core..= host),
                  Core.Just ("HTTPMethod" Core..= hTTPMethod),
                  Core.Just ("URLPath" Core..= uRLPath),
                  Core.Just ("Version" Core..= version),
                  ("Attributes" Core..=) Core.<$> attributes,
                  ("RuleARN" Core..=) Core.<$> ruleARN,
                  ("RuleName" Core..=) Core.<$> ruleName])

instance Core.FromJSON SamplingRule where
        parseJSON
          = Core.withObject "SamplingRule" Core.$
              \ x ->
                SamplingRule' Core.<$>
                  (x Core..: "ResourceARN") Core.<*> x Core..: "Priority" Core.<*>
                    x Core..: "FixedRate"
                    Core.<*> x Core..: "ReservoirSize"
                    Core.<*> x Core..: "ServiceName"
                    Core.<*> x Core..: "ServiceType"
                    Core.<*> x Core..: "Host"
                    Core.<*> x Core..: "HTTPMethod"
                    Core.<*> x Core..: "URLPath"
                    Core.<*> x Core..: "Version"
                    Core.<*> x Core..:? "Attributes"
                    Core.<*> x Core..:? "RuleARN"
                    Core.<*> x Core..:? "RuleName"

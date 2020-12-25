{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRule
  ( SamplingRule (..),

    -- * Smart constructor
    mkSamplingRule,

    -- * Lenses
    srResourceARN,
    srPriority,
    srFixedRate,
    srReservoirSize,
    srServiceName,
    srServiceType,
    srHost,
    srHTTPMethod,
    srURLPath,
    srVersion,
    srAttributes,
    srRuleARN,
    srRuleName,
  )
where

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
import qualified Network.AWS.XRay.Types.String as Types
import qualified Network.AWS.XRay.Types.URLPath as Types

-- | A sampling rule that services use to decide whether to instrument a request. Rule fields can match properties of the service, or properties of a request. The service can ignore rules that don't match its properties.
--
-- /See:/ 'mkSamplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { -- | Matches the ARN of the AWS resource on which the service runs.
    resourceARN :: Types.ResourceARN,
    -- | The priority of the sampling rule.
    priority :: Core.Natural,
    -- | The percentage of matching requests to instrument, after the reservoir is exhausted.
    fixedRate :: Core.Double,
    -- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
    reservoirSize :: Core.Natural,
    -- | Matches the @name@ that the service uses to identify itself in segments.
    serviceName :: Types.ServiceName,
    -- | Matches the @origin@ that the service uses to identify its type in segments.
    serviceType :: Types.ServiceType,
    -- | Matches the hostname from a request URL.
    host :: Types.Host,
    -- | Matches the HTTP method of a request.
    hTTPMethod :: Types.HTTPMethod,
    -- | Matches the path from a request URL.
    uRLPath :: Types.URLPath,
    -- | The version of the sampling rule format (@1@ ).
    version :: Core.Natural,
    -- | Matches attributes derived from the request.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue),
    -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
    ruleARN :: Core.Maybe Types.String,
    -- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
    ruleName :: Core.Maybe Types.RuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SamplingRule' value with any optional fields omitted.
mkSamplingRule ::
  -- | 'resourceARN'
  Types.ResourceARN ->
  -- | 'priority'
  Core.Natural ->
  -- | 'fixedRate'
  Core.Double ->
  -- | 'reservoirSize'
  Core.Natural ->
  -- | 'serviceName'
  Types.ServiceName ->
  -- | 'serviceType'
  Types.ServiceType ->
  -- | 'host'
  Types.Host ->
  -- | 'hTTPMethod'
  Types.HTTPMethod ->
  -- | 'uRLPath'
  Types.URLPath ->
  -- | 'version'
  Core.Natural ->
  SamplingRule
mkSamplingRule
  resourceARN
  priority
  fixedRate
  reservoirSize
  serviceName
  serviceType
  host
  hTTPMethod
  uRLPath
  version =
    SamplingRule'
      { resourceARN,
        priority,
        fixedRate,
        reservoirSize,
        serviceName,
        serviceType,
        host,
        hTTPMethod,
        uRLPath,
        version,
        attributes = Core.Nothing,
        ruleARN = Core.Nothing,
        ruleName = Core.Nothing
      }

-- | Matches the ARN of the AWS resource on which the service runs.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceARN :: Lens.Lens' SamplingRule Types.ResourceARN
srResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED srResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The priority of the sampling rule.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPriority :: Lens.Lens' SamplingRule Core.Natural
srPriority = Lens.field @"priority"
{-# DEPRECATED srPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srFixedRate :: Lens.Lens' SamplingRule Core.Double
srFixedRate = Lens.field @"fixedRate"
{-# DEPRECATED srFixedRate "Use generic-lens or generic-optics with 'fixedRate' instead." #-}

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- /Note:/ Consider using 'reservoirSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srReservoirSize :: Lens.Lens' SamplingRule Core.Natural
srReservoirSize = Lens.field @"reservoirSize"
{-# DEPRECATED srReservoirSize "Use generic-lens or generic-optics with 'reservoirSize' instead." #-}

-- | Matches the @name@ that the service uses to identify itself in segments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceName :: Lens.Lens' SamplingRule Types.ServiceName
srServiceName = Lens.field @"serviceName"
{-# DEPRECATED srServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | Matches the @origin@ that the service uses to identify its type in segments.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceType :: Lens.Lens' SamplingRule Types.ServiceType
srServiceType = Lens.field @"serviceType"
{-# DEPRECATED srServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Matches the hostname from a request URL.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHost :: Lens.Lens' SamplingRule Types.Host
srHost = Lens.field @"host"
{-# DEPRECATED srHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | Matches the HTTP method of a request.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHTTPMethod :: Lens.Lens' SamplingRule Types.HTTPMethod
srHTTPMethod = Lens.field @"hTTPMethod"
{-# DEPRECATED srHTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead." #-}

-- | Matches the path from a request URL.
--
-- /Note:/ Consider using 'uRLPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srURLPath :: Lens.Lens' SamplingRule Types.URLPath
srURLPath = Lens.field @"uRLPath"
{-# DEPRECATED srURLPath "Use generic-lens or generic-optics with 'uRLPath' instead." #-}

-- | The version of the sampling rule format (@1@ ).
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srVersion :: Lens.Lens' SamplingRule Core.Natural
srVersion = Lens.field @"version"
{-# DEPRECATED srVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Matches attributes derived from the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAttributes :: Lens.Lens' SamplingRule (Core.Maybe (Core.HashMap Types.AttributeKey Types.AttributeValue))
srAttributes = Lens.field @"attributes"
{-# DEPRECATED srAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleARN :: Lens.Lens' SamplingRule (Core.Maybe Types.String)
srRuleARN = Lens.field @"ruleARN"
{-# DEPRECATED srRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleName :: Lens.Lens' SamplingRule (Core.Maybe Types.RuleName)
srRuleName = Lens.field @"ruleName"
{-# DEPRECATED srRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.FromJSON SamplingRule where
  toJSON SamplingRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
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
            ("RuleName" Core..=) Core.<$> ruleName
          ]
      )

instance Core.FromJSON SamplingRule where
  parseJSON =
    Core.withObject "SamplingRule" Core.$
      \x ->
        SamplingRule'
          Core.<$> (x Core..: "ResourceARN")
          Core.<*> (x Core..: "Priority")
          Core.<*> (x Core..: "FixedRate")
          Core.<*> (x Core..: "ReservoirSize")
          Core.<*> (x Core..: "ServiceName")
          Core.<*> (x Core..: "ServiceType")
          Core.<*> (x Core..: "Host")
          Core.<*> (x Core..: "HTTPMethod")
          Core.<*> (x Core..: "URLPath")
          Core.<*> (x Core..: "Version")
          Core.<*> (x Core..:? "Attributes")
          Core.<*> (x Core..:? "RuleARN")
          Core.<*> (x Core..:? "RuleName")

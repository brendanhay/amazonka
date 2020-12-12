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
    srRuleName,
    srAttributes,
    srRuleARN,
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
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A sampling rule that services use to decide whether to instrument a request. Rule fields can match properties of the service, or properties of a request. The service can ignore rules that don't match its properties.
--
-- /See:/ 'mkSamplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { ruleName :: Lude.Maybe Lude.Text,
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    ruleARN :: Lude.Maybe Lude.Text,
    resourceARN :: Lude.Text,
    priority :: Lude.Natural,
    fixedRate :: Lude.Double,
    reservoirSize :: Lude.Natural,
    serviceName :: Lude.Text,
    serviceType :: Lude.Text,
    host :: Lude.Text,
    hTTPMethod :: Lude.Text,
    urlPath :: Lude.Text,
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

-- | Creates a value of 'SamplingRule' with the minimum fields required to make a request.
--
-- * 'attributes' - Matches attributes derived from the request.
-- * 'fixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
-- * 'hTTPMethod' - Matches the HTTP method of a request.
-- * 'host' - Matches the hostname from a request URL.
-- * 'priority' - The priority of the sampling rule.
-- * 'reservoirSize' - A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
-- * 'resourceARN' - Matches the ARN of the AWS resource on which the service runs.
-- * 'ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
-- * 'ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
-- * 'serviceName' - Matches the @name@ that the service uses to identify itself in segments.
-- * 'serviceType' - Matches the @origin@ that the service uses to identify its type in segments.
-- * 'urlPath' - Matches the path from a request URL.
-- * 'version' - The version of the sampling rule format (@1@ ).
mkSamplingRule ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'priority'
  Lude.Natural ->
  -- | 'fixedRate'
  Lude.Double ->
  -- | 'reservoirSize'
  Lude.Natural ->
  -- | 'serviceName'
  Lude.Text ->
  -- | 'serviceType'
  Lude.Text ->
  -- | 'host'
  Lude.Text ->
  -- | 'hTTPMethod'
  Lude.Text ->
  -- | 'urlPath'
  Lude.Text ->
  -- | 'version'
  Lude.Natural ->
  SamplingRule
mkSamplingRule
  pResourceARN_
  pPriority_
  pFixedRate_
  pReservoirSize_
  pServiceName_
  pServiceType_
  pHost_
  pHTTPMethod_
  pURLPath_
  pVersion_ =
    SamplingRule'
      { ruleName = Lude.Nothing,
        attributes = Lude.Nothing,
        ruleARN = Lude.Nothing,
        resourceARN = pResourceARN_,
        priority = pPriority_,
        fixedRate = pFixedRate_,
        reservoirSize = pReservoirSize_,
        serviceName = pServiceName_,
        serviceType = pServiceType_,
        host = pHost_,
        hTTPMethod = pHTTPMethod_,
        urlPath = pURLPath_,
        version = pVersion_
      }

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleName :: Lens.Lens' SamplingRule (Lude.Maybe Lude.Text)
srRuleName = Lens.lens (ruleName :: SamplingRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: SamplingRule)
{-# DEPRECATED srRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | Matches attributes derived from the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAttributes :: Lens.Lens' SamplingRule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
srAttributes = Lens.lens (attributes :: SamplingRule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: SamplingRule)
{-# DEPRECATED srAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRuleARN :: Lens.Lens' SamplingRule (Lude.Maybe Lude.Text)
srRuleARN = Lens.lens (ruleARN :: SamplingRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: SamplingRule)
{-# DEPRECATED srRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | Matches the ARN of the AWS resource on which the service runs.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceARN :: Lens.Lens' SamplingRule Lude.Text
srResourceARN = Lens.lens (resourceARN :: SamplingRule -> Lude.Text) (\s a -> s {resourceARN = a} :: SamplingRule)
{-# DEPRECATED srResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The priority of the sampling rule.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPriority :: Lens.Lens' SamplingRule Lude.Natural
srPriority = Lens.lens (priority :: SamplingRule -> Lude.Natural) (\s a -> s {priority = a} :: SamplingRule)
{-# DEPRECATED srPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srFixedRate :: Lens.Lens' SamplingRule Lude.Double
srFixedRate = Lens.lens (fixedRate :: SamplingRule -> Lude.Double) (\s a -> s {fixedRate = a} :: SamplingRule)
{-# DEPRECATED srFixedRate "Use generic-lens or generic-optics with 'fixedRate' instead." #-}

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- /Note:/ Consider using 'reservoirSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srReservoirSize :: Lens.Lens' SamplingRule Lude.Natural
srReservoirSize = Lens.lens (reservoirSize :: SamplingRule -> Lude.Natural) (\s a -> s {reservoirSize = a} :: SamplingRule)
{-# DEPRECATED srReservoirSize "Use generic-lens or generic-optics with 'reservoirSize' instead." #-}

-- | Matches the @name@ that the service uses to identify itself in segments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceName :: Lens.Lens' SamplingRule Lude.Text
srServiceName = Lens.lens (serviceName :: SamplingRule -> Lude.Text) (\s a -> s {serviceName = a} :: SamplingRule)
{-# DEPRECATED srServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | Matches the @origin@ that the service uses to identify its type in segments.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srServiceType :: Lens.Lens' SamplingRule Lude.Text
srServiceType = Lens.lens (serviceType :: SamplingRule -> Lude.Text) (\s a -> s {serviceType = a} :: SamplingRule)
{-# DEPRECATED srServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Matches the hostname from a request URL.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHost :: Lens.Lens' SamplingRule Lude.Text
srHost = Lens.lens (host :: SamplingRule -> Lude.Text) (\s a -> s {host = a} :: SamplingRule)
{-# DEPRECATED srHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | Matches the HTTP method of a request.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srHTTPMethod :: Lens.Lens' SamplingRule Lude.Text
srHTTPMethod = Lens.lens (hTTPMethod :: SamplingRule -> Lude.Text) (\s a -> s {hTTPMethod = a} :: SamplingRule)
{-# DEPRECATED srHTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead." #-}

-- | Matches the path from a request URL.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srURLPath :: Lens.Lens' SamplingRule Lude.Text
srURLPath = Lens.lens (urlPath :: SamplingRule -> Lude.Text) (\s a -> s {urlPath = a} :: SamplingRule)
{-# DEPRECATED srURLPath "Use generic-lens or generic-optics with 'urlPath' instead." #-}

-- | The version of the sampling rule format (@1@ ).
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srVersion :: Lens.Lens' SamplingRule Lude.Natural
srVersion = Lens.lens (version :: SamplingRule -> Lude.Natural) (\s a -> s {version = a} :: SamplingRule)
{-# DEPRECATED srVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON SamplingRule where
  parseJSON =
    Lude.withObject
      "SamplingRule"
      ( \x ->
          SamplingRule'
            Lude.<$> (x Lude..:? "RuleName")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RuleARN")
            Lude.<*> (x Lude..: "ResourceARN")
            Lude.<*> (x Lude..: "Priority")
            Lude.<*> (x Lude..: "FixedRate")
            Lude.<*> (x Lude..: "ReservoirSize")
            Lude.<*> (x Lude..: "ServiceName")
            Lude.<*> (x Lude..: "ServiceType")
            Lude.<*> (x Lude..: "Host")
            Lude.<*> (x Lude..: "HTTPMethod")
            Lude.<*> (x Lude..: "URLPath")
            Lude.<*> (x Lude..: "Version")
      )

instance Lude.ToJSON SamplingRule where
  toJSON SamplingRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RuleName" Lude..=) Lude.<$> ruleName,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("RuleARN" Lude..=) Lude.<$> ruleARN,
            Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("Priority" Lude..= priority),
            Lude.Just ("FixedRate" Lude..= fixedRate),
            Lude.Just ("ReservoirSize" Lude..= reservoirSize),
            Lude.Just ("ServiceName" Lude..= serviceName),
            Lude.Just ("ServiceType" Lude..= serviceType),
            Lude.Just ("Host" Lude..= host),
            Lude.Just ("HTTPMethod" Lude..= hTTPMethod),
            Lude.Just ("URLPath" Lude..= urlPath),
            Lude.Just ("Version" Lude..= version)
          ]
      )

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleUpdate
  ( SamplingRuleUpdate (..),

    -- * Smart constructor
    mkSamplingRuleUpdate,

    -- * Lenses
    sruHTTPMethod,
    sruPriority,
    sruRuleName,
    sruReservoirSize,
    sruFixedRate,
    sruResourceARN,
    sruAttributes,
    sruServiceName,
    sruServiceType,
    sruHost,
    sruRuleARN,
    sruURLPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A document specifying changes to a sampling rule's configuration.
--
-- /See:/ 'mkSamplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { hTTPMethod ::
      Lude.Maybe Lude.Text,
    priority :: Lude.Maybe Lude.Int,
    ruleName :: Lude.Maybe Lude.Text,
    reservoirSize :: Lude.Maybe Lude.Int,
    fixedRate :: Lude.Maybe Lude.Double,
    resourceARN :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    serviceName :: Lude.Maybe Lude.Text,
    serviceType :: Lude.Maybe Lude.Text,
    host :: Lude.Maybe Lude.Text,
    ruleARN :: Lude.Maybe Lude.Text,
    urlPath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingRuleUpdate' with the minimum fields required to make a request.
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
mkSamplingRuleUpdate ::
  SamplingRuleUpdate
mkSamplingRuleUpdate =
  SamplingRuleUpdate'
    { hTTPMethod = Lude.Nothing,
      priority = Lude.Nothing,
      ruleName = Lude.Nothing,
      reservoirSize = Lude.Nothing,
      fixedRate = Lude.Nothing,
      resourceARN = Lude.Nothing,
      attributes = Lude.Nothing,
      serviceName = Lude.Nothing,
      serviceType = Lude.Nothing,
      host = Lude.Nothing,
      ruleARN = Lude.Nothing,
      urlPath = Lude.Nothing
    }

-- | Matches the HTTP method of a request.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruHTTPMethod :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruHTTPMethod = Lens.lens (hTTPMethod :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {hTTPMethod = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruHTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead." #-}

-- | The priority of the sampling rule.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruPriority :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Int)
sruPriority = Lens.lens (priority :: SamplingRuleUpdate -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruRuleName :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruRuleName = Lens.lens (ruleName :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | A fixed number of matching requests to instrument per second, prior to applying the fixed rate. The reservoir is not used directly by services, but applies to all services using the rule collectively.
--
-- /Note:/ Consider using 'reservoirSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruReservoirSize :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Int)
sruReservoirSize = Lens.lens (reservoirSize :: SamplingRuleUpdate -> Lude.Maybe Lude.Int) (\s a -> s {reservoirSize = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruReservoirSize "Use generic-lens or generic-optics with 'reservoirSize' instead." #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruFixedRate :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Double)
sruFixedRate = Lens.lens (fixedRate :: SamplingRuleUpdate -> Lude.Maybe Lude.Double) (\s a -> s {fixedRate = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruFixedRate "Use generic-lens or generic-optics with 'fixedRate' instead." #-}

-- | Matches the ARN of the AWS resource on which the service runs.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruResourceARN :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruResourceARN = Lens.lens (resourceARN :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Matches attributes derived from the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruAttributes :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sruAttributes = Lens.lens (attributes :: SamplingRuleUpdate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Matches the @name@ that the service uses to identify itself in segments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruServiceName :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruServiceName = Lens.lens (serviceName :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | Matches the @origin@ that the service uses to identify its type in segments.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruServiceType :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruServiceType = Lens.lens (serviceType :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {serviceType = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

-- | Matches the hostname from a request URL.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruHost :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruHost = Lens.lens (host :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {host = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruRuleARN :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruRuleARN = Lens.lens (ruleARN :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | Matches the path from a request URL.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sruURLPath :: Lens.Lens' SamplingRuleUpdate (Lude.Maybe Lude.Text)
sruURLPath = Lens.lens (urlPath :: SamplingRuleUpdate -> Lude.Maybe Lude.Text) (\s a -> s {urlPath = a} :: SamplingRuleUpdate)
{-# DEPRECATED sruURLPath "Use generic-lens or generic-optics with 'urlPath' instead." #-}

instance Lude.ToJSON SamplingRuleUpdate where
  toJSON SamplingRuleUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HTTPMethod" Lude..=) Lude.<$> hTTPMethod,
            ("Priority" Lude..=) Lude.<$> priority,
            ("RuleName" Lude..=) Lude.<$> ruleName,
            ("ReservoirSize" Lude..=) Lude.<$> reservoirSize,
            ("FixedRate" Lude..=) Lude.<$> fixedRate,
            ("ResourceARN" Lude..=) Lude.<$> resourceARN,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("ServiceName" Lude..=) Lude.<$> serviceName,
            ("ServiceType" Lude..=) Lude.<$> serviceType,
            ("Host" Lude..=) Lude.<$> host,
            ("RuleARN" Lude..=) Lude.<$> ruleARN,
            ("URLPath" Lude..=) Lude.<$> urlPath
          ]
      )

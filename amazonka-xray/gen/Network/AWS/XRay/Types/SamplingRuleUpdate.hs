{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A document specifying changes to a sampling rule\'s configuration.
--
-- /See:/ 'newSamplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { -- | Matches the ARN of the AWS resource on which the service runs.
    resourceARN :: Core.Maybe Core.Text,
    -- | Matches the HTTP method of a request.
    hTTPMethod :: Core.Maybe Core.Text,
    -- | A fixed number of matching requests to instrument per second, prior to
    -- applying the fixed rate. The reservoir is not used directly by services,
    -- but applies to all services using the rule collectively.
    reservoirSize :: Core.Maybe Core.Int,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Core.Maybe Core.Double,
    -- | The name of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleName :: Core.Maybe Core.Text,
    -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleARN :: Core.Maybe Core.Text,
    -- | Matches the @name@ that the service uses to identify itself in segments.
    serviceName :: Core.Maybe Core.Text,
    -- | The priority of the sampling rule.
    priority :: Core.Maybe Core.Int,
    -- | Matches attributes derived from the request.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Matches the path from a request URL.
    uRLPath :: Core.Maybe Core.Text,
    -- | Matches the hostname from a request URL.
    host :: Core.Maybe Core.Text,
    -- | Matches the @origin@ that the service uses to identify its type in
    -- segments.
    serviceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SamplingRuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'samplingRuleUpdate_resourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- 'hTTPMethod', 'samplingRuleUpdate_hTTPMethod' - Matches the HTTP method of a request.
--
-- 'reservoirSize', 'samplingRuleUpdate_reservoirSize' - A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
--
-- 'fixedRate', 'samplingRuleUpdate_fixedRate' - The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
--
-- 'ruleName', 'samplingRuleUpdate_ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'ruleARN', 'samplingRuleUpdate_ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'serviceName', 'samplingRuleUpdate_serviceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- 'priority', 'samplingRuleUpdate_priority' - The priority of the sampling rule.
--
-- 'attributes', 'samplingRuleUpdate_attributes' - Matches attributes derived from the request.
--
-- 'uRLPath', 'samplingRuleUpdate_uRLPath' - Matches the path from a request URL.
--
-- 'host', 'samplingRuleUpdate_host' - Matches the hostname from a request URL.
--
-- 'serviceType', 'samplingRuleUpdate_serviceType' - Matches the @origin@ that the service uses to identify its type in
-- segments.
newSamplingRuleUpdate ::
  SamplingRuleUpdate
newSamplingRuleUpdate =
  SamplingRuleUpdate'
    { resourceARN = Core.Nothing,
      hTTPMethod = Core.Nothing,
      reservoirSize = Core.Nothing,
      fixedRate = Core.Nothing,
      ruleName = Core.Nothing,
      ruleARN = Core.Nothing,
      serviceName = Core.Nothing,
      priority = Core.Nothing,
      attributes = Core.Nothing,
      uRLPath = Core.Nothing,
      host = Core.Nothing,
      serviceType = Core.Nothing
    }

-- | Matches the ARN of the AWS resource on which the service runs.
samplingRuleUpdate_resourceARN :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_resourceARN = Lens.lens (\SamplingRuleUpdate' {resourceARN} -> resourceARN) (\s@SamplingRuleUpdate' {} a -> s {resourceARN = a} :: SamplingRuleUpdate)

-- | Matches the HTTP method of a request.
samplingRuleUpdate_hTTPMethod :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_hTTPMethod = Lens.lens (\SamplingRuleUpdate' {hTTPMethod} -> hTTPMethod) (\s@SamplingRuleUpdate' {} a -> s {hTTPMethod = a} :: SamplingRuleUpdate)

-- | A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
samplingRuleUpdate_reservoirSize :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Int)
samplingRuleUpdate_reservoirSize = Lens.lens (\SamplingRuleUpdate' {reservoirSize} -> reservoirSize) (\s@SamplingRuleUpdate' {} a -> s {reservoirSize = a} :: SamplingRuleUpdate)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingRuleUpdate_fixedRate :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Double)
samplingRuleUpdate_fixedRate = Lens.lens (\SamplingRuleUpdate' {fixedRate} -> fixedRate) (\s@SamplingRuleUpdate' {} a -> s {fixedRate = a} :: SamplingRuleUpdate)

-- | The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRuleUpdate_ruleName :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_ruleName = Lens.lens (\SamplingRuleUpdate' {ruleName} -> ruleName) (\s@SamplingRuleUpdate' {} a -> s {ruleName = a} :: SamplingRuleUpdate)

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRuleUpdate_ruleARN :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_ruleARN = Lens.lens (\SamplingRuleUpdate' {ruleARN} -> ruleARN) (\s@SamplingRuleUpdate' {} a -> s {ruleARN = a} :: SamplingRuleUpdate)

-- | Matches the @name@ that the service uses to identify itself in segments.
samplingRuleUpdate_serviceName :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_serviceName = Lens.lens (\SamplingRuleUpdate' {serviceName} -> serviceName) (\s@SamplingRuleUpdate' {} a -> s {serviceName = a} :: SamplingRuleUpdate)

-- | The priority of the sampling rule.
samplingRuleUpdate_priority :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Int)
samplingRuleUpdate_priority = Lens.lens (\SamplingRuleUpdate' {priority} -> priority) (\s@SamplingRuleUpdate' {} a -> s {priority = a} :: SamplingRuleUpdate)

-- | Matches attributes derived from the request.
samplingRuleUpdate_attributes :: Lens.Lens' SamplingRuleUpdate (Core.Maybe (Core.HashMap Core.Text Core.Text))
samplingRuleUpdate_attributes = Lens.lens (\SamplingRuleUpdate' {attributes} -> attributes) (\s@SamplingRuleUpdate' {} a -> s {attributes = a} :: SamplingRuleUpdate) Core.. Lens.mapping Lens._Coerce

-- | Matches the path from a request URL.
samplingRuleUpdate_uRLPath :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_uRLPath = Lens.lens (\SamplingRuleUpdate' {uRLPath} -> uRLPath) (\s@SamplingRuleUpdate' {} a -> s {uRLPath = a} :: SamplingRuleUpdate)

-- | Matches the hostname from a request URL.
samplingRuleUpdate_host :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_host = Lens.lens (\SamplingRuleUpdate' {host} -> host) (\s@SamplingRuleUpdate' {} a -> s {host = a} :: SamplingRuleUpdate)

-- | Matches the @origin@ that the service uses to identify its type in
-- segments.
samplingRuleUpdate_serviceType :: Lens.Lens' SamplingRuleUpdate (Core.Maybe Core.Text)
samplingRuleUpdate_serviceType = Lens.lens (\SamplingRuleUpdate' {serviceType} -> serviceType) (\s@SamplingRuleUpdate' {} a -> s {serviceType = a} :: SamplingRuleUpdate)

instance Core.Hashable SamplingRuleUpdate

instance Core.NFData SamplingRuleUpdate

instance Core.ToJSON SamplingRuleUpdate where
  toJSON SamplingRuleUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARN" Core..=) Core.<$> resourceARN,
            ("HTTPMethod" Core..=) Core.<$> hTTPMethod,
            ("ReservoirSize" Core..=) Core.<$> reservoirSize,
            ("FixedRate" Core..=) Core.<$> fixedRate,
            ("RuleName" Core..=) Core.<$> ruleName,
            ("RuleARN" Core..=) Core.<$> ruleARN,
            ("ServiceName" Core..=) Core.<$> serviceName,
            ("Priority" Core..=) Core.<$> priority,
            ("Attributes" Core..=) Core.<$> attributes,
            ("URLPath" Core..=) Core.<$> uRLPath,
            ("Host" Core..=) Core.<$> host,
            ("ServiceType" Core..=) Core.<$> serviceType
          ]
      )

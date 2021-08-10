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
import qualified Network.AWS.Prelude as Prelude

-- | A document specifying changes to a sampling rule\'s configuration.
--
-- /See:/ 'newSamplingRuleUpdate' smart constructor.
data SamplingRuleUpdate = SamplingRuleUpdate'
  { -- | Matches the ARN of the AWS resource on which the service runs.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | Matches the HTTP method of a request.
    hTTPMethod :: Prelude.Maybe Prelude.Text,
    -- | A fixed number of matching requests to instrument per second, prior to
    -- applying the fixed rate. The reservoir is not used directly by services,
    -- but applies to all services using the rule collectively.
    reservoirSize :: Prelude.Maybe Prelude.Int,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Prelude.Maybe Prelude.Double,
    -- | The name of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleARN :: Prelude.Maybe Prelude.Text,
    -- | Matches the @name@ that the service uses to identify itself in segments.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The priority of the sampling rule.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Matches attributes derived from the request.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Matches the path from a request URL.
    uRLPath :: Prelude.Maybe Prelude.Text,
    -- | Matches the hostname from a request URL.
    host :: Prelude.Maybe Prelude.Text,
    -- | Matches the @origin@ that the service uses to identify its type in
    -- segments.
    serviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { resourceARN = Prelude.Nothing,
      hTTPMethod = Prelude.Nothing,
      reservoirSize = Prelude.Nothing,
      fixedRate = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      ruleARN = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      priority = Prelude.Nothing,
      attributes = Prelude.Nothing,
      uRLPath = Prelude.Nothing,
      host = Prelude.Nothing,
      serviceType = Prelude.Nothing
    }

-- | Matches the ARN of the AWS resource on which the service runs.
samplingRuleUpdate_resourceARN :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_resourceARN = Lens.lens (\SamplingRuleUpdate' {resourceARN} -> resourceARN) (\s@SamplingRuleUpdate' {} a -> s {resourceARN = a} :: SamplingRuleUpdate)

-- | Matches the HTTP method of a request.
samplingRuleUpdate_hTTPMethod :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_hTTPMethod = Lens.lens (\SamplingRuleUpdate' {hTTPMethod} -> hTTPMethod) (\s@SamplingRuleUpdate' {} a -> s {hTTPMethod = a} :: SamplingRuleUpdate)

-- | A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
samplingRuleUpdate_reservoirSize :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Int)
samplingRuleUpdate_reservoirSize = Lens.lens (\SamplingRuleUpdate' {reservoirSize} -> reservoirSize) (\s@SamplingRuleUpdate' {} a -> s {reservoirSize = a} :: SamplingRuleUpdate)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingRuleUpdate_fixedRate :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Double)
samplingRuleUpdate_fixedRate = Lens.lens (\SamplingRuleUpdate' {fixedRate} -> fixedRate) (\s@SamplingRuleUpdate' {} a -> s {fixedRate = a} :: SamplingRuleUpdate)

-- | The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRuleUpdate_ruleName :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_ruleName = Lens.lens (\SamplingRuleUpdate' {ruleName} -> ruleName) (\s@SamplingRuleUpdate' {} a -> s {ruleName = a} :: SamplingRuleUpdate)

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRuleUpdate_ruleARN :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_ruleARN = Lens.lens (\SamplingRuleUpdate' {ruleARN} -> ruleARN) (\s@SamplingRuleUpdate' {} a -> s {ruleARN = a} :: SamplingRuleUpdate)

-- | Matches the @name@ that the service uses to identify itself in segments.
samplingRuleUpdate_serviceName :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_serviceName = Lens.lens (\SamplingRuleUpdate' {serviceName} -> serviceName) (\s@SamplingRuleUpdate' {} a -> s {serviceName = a} :: SamplingRuleUpdate)

-- | The priority of the sampling rule.
samplingRuleUpdate_priority :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Int)
samplingRuleUpdate_priority = Lens.lens (\SamplingRuleUpdate' {priority} -> priority) (\s@SamplingRuleUpdate' {} a -> s {priority = a} :: SamplingRuleUpdate)

-- | Matches attributes derived from the request.
samplingRuleUpdate_attributes :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
samplingRuleUpdate_attributes = Lens.lens (\SamplingRuleUpdate' {attributes} -> attributes) (\s@SamplingRuleUpdate' {} a -> s {attributes = a} :: SamplingRuleUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | Matches the path from a request URL.
samplingRuleUpdate_uRLPath :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_uRLPath = Lens.lens (\SamplingRuleUpdate' {uRLPath} -> uRLPath) (\s@SamplingRuleUpdate' {} a -> s {uRLPath = a} :: SamplingRuleUpdate)

-- | Matches the hostname from a request URL.
samplingRuleUpdate_host :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_host = Lens.lens (\SamplingRuleUpdate' {host} -> host) (\s@SamplingRuleUpdate' {} a -> s {host = a} :: SamplingRuleUpdate)

-- | Matches the @origin@ that the service uses to identify its type in
-- segments.
samplingRuleUpdate_serviceType :: Lens.Lens' SamplingRuleUpdate (Prelude.Maybe Prelude.Text)
samplingRuleUpdate_serviceType = Lens.lens (\SamplingRuleUpdate' {serviceType} -> serviceType) (\s@SamplingRuleUpdate' {} a -> s {serviceType = a} :: SamplingRuleUpdate)

instance Prelude.Hashable SamplingRuleUpdate

instance Prelude.NFData SamplingRuleUpdate

instance Core.ToJSON SamplingRuleUpdate where
  toJSON SamplingRuleUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceARN" Core..=) Prelude.<$> resourceARN,
            ("HTTPMethod" Core..=) Prelude.<$> hTTPMethod,
            ("ReservoirSize" Core..=) Prelude.<$> reservoirSize,
            ("FixedRate" Core..=) Prelude.<$> fixedRate,
            ("RuleName" Core..=) Prelude.<$> ruleName,
            ("RuleARN" Core..=) Prelude.<$> ruleARN,
            ("ServiceName" Core..=) Prelude.<$> serviceName,
            ("Priority" Core..=) Prelude.<$> priority,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("URLPath" Core..=) Prelude.<$> uRLPath,
            ("Host" Core..=) Prelude.<$> host,
            ("ServiceType" Core..=) Prelude.<$> serviceType
          ]
      )

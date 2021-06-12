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
-- Module      : Network.AWS.XRay.Types.SamplingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A sampling rule that services use to decide whether to instrument a
-- request. Rule fields can match properties of the service, or properties
-- of a request. The service can ignore rules that don\'t match its
-- properties.
--
-- /See:/ 'newSamplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { -- | The name of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleName :: Core.Maybe Core.Text,
    -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleARN :: Core.Maybe Core.Text,
    -- | Matches attributes derived from the request.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Matches the ARN of the AWS resource on which the service runs.
    resourceARN :: Core.Text,
    -- | The priority of the sampling rule.
    priority :: Core.Natural,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Core.Double,
    -- | A fixed number of matching requests to instrument per second, prior to
    -- applying the fixed rate. The reservoir is not used directly by services,
    -- but applies to all services using the rule collectively.
    reservoirSize :: Core.Natural,
    -- | Matches the @name@ that the service uses to identify itself in segments.
    serviceName :: Core.Text,
    -- | Matches the @origin@ that the service uses to identify its type in
    -- segments.
    serviceType :: Core.Text,
    -- | Matches the hostname from a request URL.
    host :: Core.Text,
    -- | Matches the HTTP method of a request.
    hTTPMethod :: Core.Text,
    -- | Matches the path from a request URL.
    uRLPath :: Core.Text,
    -- | The version of the sampling rule format (@1@).
    version :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'samplingRule_ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'ruleARN', 'samplingRule_ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'attributes', 'samplingRule_attributes' - Matches attributes derived from the request.
--
-- 'resourceARN', 'samplingRule_resourceARN' - Matches the ARN of the AWS resource on which the service runs.
--
-- 'priority', 'samplingRule_priority' - The priority of the sampling rule.
--
-- 'fixedRate', 'samplingRule_fixedRate' - The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
--
-- 'reservoirSize', 'samplingRule_reservoirSize' - A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
--
-- 'serviceName', 'samplingRule_serviceName' - Matches the @name@ that the service uses to identify itself in segments.
--
-- 'serviceType', 'samplingRule_serviceType' - Matches the @origin@ that the service uses to identify its type in
-- segments.
--
-- 'host', 'samplingRule_host' - Matches the hostname from a request URL.
--
-- 'hTTPMethod', 'samplingRule_hTTPMethod' - Matches the HTTP method of a request.
--
-- 'uRLPath', 'samplingRule_uRLPath' - Matches the path from a request URL.
--
-- 'version', 'samplingRule_version' - The version of the sampling rule format (@1@).
newSamplingRule ::
  -- | 'resourceARN'
  Core.Text ->
  -- | 'priority'
  Core.Natural ->
  -- | 'fixedRate'
  Core.Double ->
  -- | 'reservoirSize'
  Core.Natural ->
  -- | 'serviceName'
  Core.Text ->
  -- | 'serviceType'
  Core.Text ->
  -- | 'host'
  Core.Text ->
  -- | 'hTTPMethod'
  Core.Text ->
  -- | 'uRLPath'
  Core.Text ->
  -- | 'version'
  Core.Natural ->
  SamplingRule
newSamplingRule
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
      { ruleName = Core.Nothing,
        ruleARN = Core.Nothing,
        attributes = Core.Nothing,
        resourceARN = pResourceARN_,
        priority = pPriority_,
        fixedRate = pFixedRate_,
        reservoirSize = pReservoirSize_,
        serviceName = pServiceName_,
        serviceType = pServiceType_,
        host = pHost_,
        hTTPMethod = pHTTPMethod_,
        uRLPath = pURLPath_,
        version = pVersion_
      }

-- | The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRule_ruleName :: Lens.Lens' SamplingRule (Core.Maybe Core.Text)
samplingRule_ruleName = Lens.lens (\SamplingRule' {ruleName} -> ruleName) (\s@SamplingRule' {} a -> s {ruleName = a} :: SamplingRule)

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRule_ruleARN :: Lens.Lens' SamplingRule (Core.Maybe Core.Text)
samplingRule_ruleARN = Lens.lens (\SamplingRule' {ruleARN} -> ruleARN) (\s@SamplingRule' {} a -> s {ruleARN = a} :: SamplingRule)

-- | Matches attributes derived from the request.
samplingRule_attributes :: Lens.Lens' SamplingRule (Core.Maybe (Core.HashMap Core.Text Core.Text))
samplingRule_attributes = Lens.lens (\SamplingRule' {attributes} -> attributes) (\s@SamplingRule' {} a -> s {attributes = a} :: SamplingRule) Core.. Lens.mapping Lens._Coerce

-- | Matches the ARN of the AWS resource on which the service runs.
samplingRule_resourceARN :: Lens.Lens' SamplingRule Core.Text
samplingRule_resourceARN = Lens.lens (\SamplingRule' {resourceARN} -> resourceARN) (\s@SamplingRule' {} a -> s {resourceARN = a} :: SamplingRule)

-- | The priority of the sampling rule.
samplingRule_priority :: Lens.Lens' SamplingRule Core.Natural
samplingRule_priority = Lens.lens (\SamplingRule' {priority} -> priority) (\s@SamplingRule' {} a -> s {priority = a} :: SamplingRule)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingRule_fixedRate :: Lens.Lens' SamplingRule Core.Double
samplingRule_fixedRate = Lens.lens (\SamplingRule' {fixedRate} -> fixedRate) (\s@SamplingRule' {} a -> s {fixedRate = a} :: SamplingRule)

-- | A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
samplingRule_reservoirSize :: Lens.Lens' SamplingRule Core.Natural
samplingRule_reservoirSize = Lens.lens (\SamplingRule' {reservoirSize} -> reservoirSize) (\s@SamplingRule' {} a -> s {reservoirSize = a} :: SamplingRule)

-- | Matches the @name@ that the service uses to identify itself in segments.
samplingRule_serviceName :: Lens.Lens' SamplingRule Core.Text
samplingRule_serviceName = Lens.lens (\SamplingRule' {serviceName} -> serviceName) (\s@SamplingRule' {} a -> s {serviceName = a} :: SamplingRule)

-- | Matches the @origin@ that the service uses to identify its type in
-- segments.
samplingRule_serviceType :: Lens.Lens' SamplingRule Core.Text
samplingRule_serviceType = Lens.lens (\SamplingRule' {serviceType} -> serviceType) (\s@SamplingRule' {} a -> s {serviceType = a} :: SamplingRule)

-- | Matches the hostname from a request URL.
samplingRule_host :: Lens.Lens' SamplingRule Core.Text
samplingRule_host = Lens.lens (\SamplingRule' {host} -> host) (\s@SamplingRule' {} a -> s {host = a} :: SamplingRule)

-- | Matches the HTTP method of a request.
samplingRule_hTTPMethod :: Lens.Lens' SamplingRule Core.Text
samplingRule_hTTPMethod = Lens.lens (\SamplingRule' {hTTPMethod} -> hTTPMethod) (\s@SamplingRule' {} a -> s {hTTPMethod = a} :: SamplingRule)

-- | Matches the path from a request URL.
samplingRule_uRLPath :: Lens.Lens' SamplingRule Core.Text
samplingRule_uRLPath = Lens.lens (\SamplingRule' {uRLPath} -> uRLPath) (\s@SamplingRule' {} a -> s {uRLPath = a} :: SamplingRule)

-- | The version of the sampling rule format (@1@).
samplingRule_version :: Lens.Lens' SamplingRule Core.Natural
samplingRule_version = Lens.lens (\SamplingRule' {version} -> version) (\s@SamplingRule' {} a -> s {version = a} :: SamplingRule)

instance Core.FromJSON SamplingRule where
  parseJSON =
    Core.withObject
      "SamplingRule"
      ( \x ->
          SamplingRule'
            Core.<$> (x Core..:? "RuleName")
            Core.<*> (x Core..:? "RuleARN")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..: "ResourceARN")
            Core.<*> (x Core..: "Priority")
            Core.<*> (x Core..: "FixedRate")
            Core.<*> (x Core..: "ReservoirSize")
            Core.<*> (x Core..: "ServiceName")
            Core.<*> (x Core..: "ServiceType")
            Core.<*> (x Core..: "Host")
            Core.<*> (x Core..: "HTTPMethod")
            Core.<*> (x Core..: "URLPath")
            Core.<*> (x Core..: "Version")
      )

instance Core.Hashable SamplingRule

instance Core.NFData SamplingRule

instance Core.ToJSON SamplingRule where
  toJSON SamplingRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RuleName" Core..=) Core.<$> ruleName,
            ("RuleARN" Core..=) Core.<$> ruleARN,
            ("Attributes" Core..=) Core.<$> attributes,
            Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("Priority" Core..= priority),
            Core.Just ("FixedRate" Core..= fixedRate),
            Core.Just ("ReservoirSize" Core..= reservoirSize),
            Core.Just ("ServiceName" Core..= serviceName),
            Core.Just ("ServiceType" Core..= serviceType),
            Core.Just ("Host" Core..= host),
            Core.Just ("HTTPMethod" Core..= hTTPMethod),
            Core.Just ("URLPath" Core..= uRLPath),
            Core.Just ("Version" Core..= version)
          ]
      )

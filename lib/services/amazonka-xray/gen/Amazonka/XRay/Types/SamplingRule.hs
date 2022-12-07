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
-- Module      : Amazonka.XRay.Types.SamplingRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A sampling rule that services use to decide whether to instrument a
-- request. Rule fields can match properties of the service, or properties
-- of a request. The service can ignore rules that don\'t match its
-- properties.
--
-- /See:/ 'newSamplingRule' smart constructor.
data SamplingRule = SamplingRule'
  { -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | Matches attributes derived from the request.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Matches the ARN of the Amazon Web Services resource on which the service
    -- runs.
    resourceARN :: Prelude.Text,
    -- | The priority of the sampling rule.
    priority :: Prelude.Natural,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Prelude.Double,
    -- | A fixed number of matching requests to instrument per second, prior to
    -- applying the fixed rate. The reservoir is not used directly by services,
    -- but applies to all services using the rule collectively.
    reservoirSize :: Prelude.Natural,
    -- | Matches the @name@ that the service uses to identify itself in segments.
    serviceName :: Prelude.Text,
    -- | Matches the @origin@ that the service uses to identify its type in
    -- segments.
    serviceType :: Prelude.Text,
    -- | Matches the hostname from a request URL.
    host :: Prelude.Text,
    -- | Matches the HTTP method of a request.
    hTTPMethod :: Prelude.Text,
    -- | Matches the path from a request URL.
    uRLPath :: Prelude.Text,
    -- | The version of the sampling rule format (@1@).
    version :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleARN', 'samplingRule_ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'ruleName', 'samplingRule_ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'attributes', 'samplingRule_attributes' - Matches attributes derived from the request.
--
-- 'resourceARN', 'samplingRule_resourceARN' - Matches the ARN of the Amazon Web Services resource on which the service
-- runs.
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
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  -- | 'fixedRate'
  Prelude.Double ->
  -- | 'reservoirSize'
  Prelude.Natural ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceType'
  Prelude.Text ->
  -- | 'host'
  Prelude.Text ->
  -- | 'hTTPMethod'
  Prelude.Text ->
  -- | 'uRLPath'
  Prelude.Text ->
  -- | 'version'
  Prelude.Natural ->
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
      { ruleARN = Prelude.Nothing,
        ruleName = Prelude.Nothing,
        attributes = Prelude.Nothing,
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

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRule_ruleARN :: Lens.Lens' SamplingRule (Prelude.Maybe Prelude.Text)
samplingRule_ruleARN = Lens.lens (\SamplingRule' {ruleARN} -> ruleARN) (\s@SamplingRule' {} a -> s {ruleARN = a} :: SamplingRule)

-- | The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
samplingRule_ruleName :: Lens.Lens' SamplingRule (Prelude.Maybe Prelude.Text)
samplingRule_ruleName = Lens.lens (\SamplingRule' {ruleName} -> ruleName) (\s@SamplingRule' {} a -> s {ruleName = a} :: SamplingRule)

-- | Matches attributes derived from the request.
samplingRule_attributes :: Lens.Lens' SamplingRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
samplingRule_attributes = Lens.lens (\SamplingRule' {attributes} -> attributes) (\s@SamplingRule' {} a -> s {attributes = a} :: SamplingRule) Prelude.. Lens.mapping Lens.coerced

-- | Matches the ARN of the Amazon Web Services resource on which the service
-- runs.
samplingRule_resourceARN :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_resourceARN = Lens.lens (\SamplingRule' {resourceARN} -> resourceARN) (\s@SamplingRule' {} a -> s {resourceARN = a} :: SamplingRule)

-- | The priority of the sampling rule.
samplingRule_priority :: Lens.Lens' SamplingRule Prelude.Natural
samplingRule_priority = Lens.lens (\SamplingRule' {priority} -> priority) (\s@SamplingRule' {} a -> s {priority = a} :: SamplingRule)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingRule_fixedRate :: Lens.Lens' SamplingRule Prelude.Double
samplingRule_fixedRate = Lens.lens (\SamplingRule' {fixedRate} -> fixedRate) (\s@SamplingRule' {} a -> s {fixedRate = a} :: SamplingRule)

-- | A fixed number of matching requests to instrument per second, prior to
-- applying the fixed rate. The reservoir is not used directly by services,
-- but applies to all services using the rule collectively.
samplingRule_reservoirSize :: Lens.Lens' SamplingRule Prelude.Natural
samplingRule_reservoirSize = Lens.lens (\SamplingRule' {reservoirSize} -> reservoirSize) (\s@SamplingRule' {} a -> s {reservoirSize = a} :: SamplingRule)

-- | Matches the @name@ that the service uses to identify itself in segments.
samplingRule_serviceName :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_serviceName = Lens.lens (\SamplingRule' {serviceName} -> serviceName) (\s@SamplingRule' {} a -> s {serviceName = a} :: SamplingRule)

-- | Matches the @origin@ that the service uses to identify its type in
-- segments.
samplingRule_serviceType :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_serviceType = Lens.lens (\SamplingRule' {serviceType} -> serviceType) (\s@SamplingRule' {} a -> s {serviceType = a} :: SamplingRule)

-- | Matches the hostname from a request URL.
samplingRule_host :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_host = Lens.lens (\SamplingRule' {host} -> host) (\s@SamplingRule' {} a -> s {host = a} :: SamplingRule)

-- | Matches the HTTP method of a request.
samplingRule_hTTPMethod :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_hTTPMethod = Lens.lens (\SamplingRule' {hTTPMethod} -> hTTPMethod) (\s@SamplingRule' {} a -> s {hTTPMethod = a} :: SamplingRule)

-- | Matches the path from a request URL.
samplingRule_uRLPath :: Lens.Lens' SamplingRule Prelude.Text
samplingRule_uRLPath = Lens.lens (\SamplingRule' {uRLPath} -> uRLPath) (\s@SamplingRule' {} a -> s {uRLPath = a} :: SamplingRule)

-- | The version of the sampling rule format (@1@).
samplingRule_version :: Lens.Lens' SamplingRule Prelude.Natural
samplingRule_version = Lens.lens (\SamplingRule' {version} -> version) (\s@SamplingRule' {} a -> s {version = a} :: SamplingRule)

instance Data.FromJSON SamplingRule where
  parseJSON =
    Data.withObject
      "SamplingRule"
      ( \x ->
          SamplingRule'
            Prelude.<$> (x Data..:? "RuleARN")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ResourceARN")
            Prelude.<*> (x Data..: "Priority")
            Prelude.<*> (x Data..: "FixedRate")
            Prelude.<*> (x Data..: "ReservoirSize")
            Prelude.<*> (x Data..: "ServiceName")
            Prelude.<*> (x Data..: "ServiceType")
            Prelude.<*> (x Data..: "Host")
            Prelude.<*> (x Data..: "HTTPMethod")
            Prelude.<*> (x Data..: "URLPath")
            Prelude.<*> (x Data..: "Version")
      )

instance Prelude.Hashable SamplingRule where
  hashWithSalt _salt SamplingRule' {..} =
    _salt `Prelude.hashWithSalt` ruleARN
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` fixedRate
      `Prelude.hashWithSalt` reservoirSize
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` hTTPMethod
      `Prelude.hashWithSalt` uRLPath
      `Prelude.hashWithSalt` version

instance Prelude.NFData SamplingRule where
  rnf SamplingRule' {..} =
    Prelude.rnf ruleARN
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf fixedRate
      `Prelude.seq` Prelude.rnf reservoirSize
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf hTTPMethod
      `Prelude.seq` Prelude.rnf uRLPath
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON SamplingRule where
  toJSON SamplingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleARN" Data..=) Prelude.<$> ruleARN,
            ("RuleName" Data..=) Prelude.<$> ruleName,
            ("Attributes" Data..=) Prelude.<$> attributes,
            Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("FixedRate" Data..= fixedRate),
            Prelude.Just ("ReservoirSize" Data..= reservoirSize),
            Prelude.Just ("ServiceName" Data..= serviceName),
            Prelude.Just ("ServiceType" Data..= serviceType),
            Prelude.Just ("Host" Data..= host),
            Prelude.Just ("HTTPMethod" Data..= hTTPMethod),
            Prelude.Just ("URLPath" Data..= uRLPath),
            Prelude.Just ("Version" Data..= version)
          ]
      )

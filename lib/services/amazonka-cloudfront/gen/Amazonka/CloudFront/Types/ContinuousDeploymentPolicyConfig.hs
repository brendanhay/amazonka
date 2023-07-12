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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentPolicyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentPolicyConfig where

import Amazonka.CloudFront.Types.StagingDistributionDnsNames
import Amazonka.CloudFront.Types.TrafficConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration for a continuous deployment policy.
--
-- /See:/ 'newContinuousDeploymentPolicyConfig' smart constructor.
data ContinuousDeploymentPolicyConfig = ContinuousDeploymentPolicyConfig'
  { -- | Contains the parameters for routing production traffic from your primary
    -- to staging distributions.
    trafficConfig :: Prelude.Maybe TrafficConfig,
    -- | The CloudFront domain name of the staging distribution. For example:
    -- @d111111abcdef8.cloudfront.net@.
    stagingDistributionDnsNames :: StagingDistributionDnsNames,
    -- | A Boolean that indicates whether this continuous deployment policy is
    -- enabled (in effect). When this value is @true@, this policy is enabled
    -- and in effect. When this value is @false@, this policy is not enabled
    -- and has no effect.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficConfig', 'continuousDeploymentPolicyConfig_trafficConfig' - Contains the parameters for routing production traffic from your primary
-- to staging distributions.
--
-- 'stagingDistributionDnsNames', 'continuousDeploymentPolicyConfig_stagingDistributionDnsNames' - The CloudFront domain name of the staging distribution. For example:
-- @d111111abcdef8.cloudfront.net@.
--
-- 'enabled', 'continuousDeploymentPolicyConfig_enabled' - A Boolean that indicates whether this continuous deployment policy is
-- enabled (in effect). When this value is @true@, this policy is enabled
-- and in effect. When this value is @false@, this policy is not enabled
-- and has no effect.
newContinuousDeploymentPolicyConfig ::
  -- | 'stagingDistributionDnsNames'
  StagingDistributionDnsNames ->
  -- | 'enabled'
  Prelude.Bool ->
  ContinuousDeploymentPolicyConfig
newContinuousDeploymentPolicyConfig
  pStagingDistributionDnsNames_
  pEnabled_ =
    ContinuousDeploymentPolicyConfig'
      { trafficConfig =
          Prelude.Nothing,
        stagingDistributionDnsNames =
          pStagingDistributionDnsNames_,
        enabled = pEnabled_
      }

-- | Contains the parameters for routing production traffic from your primary
-- to staging distributions.
continuousDeploymentPolicyConfig_trafficConfig :: Lens.Lens' ContinuousDeploymentPolicyConfig (Prelude.Maybe TrafficConfig)
continuousDeploymentPolicyConfig_trafficConfig = Lens.lens (\ContinuousDeploymentPolicyConfig' {trafficConfig} -> trafficConfig) (\s@ContinuousDeploymentPolicyConfig' {} a -> s {trafficConfig = a} :: ContinuousDeploymentPolicyConfig)

-- | The CloudFront domain name of the staging distribution. For example:
-- @d111111abcdef8.cloudfront.net@.
continuousDeploymentPolicyConfig_stagingDistributionDnsNames :: Lens.Lens' ContinuousDeploymentPolicyConfig StagingDistributionDnsNames
continuousDeploymentPolicyConfig_stagingDistributionDnsNames = Lens.lens (\ContinuousDeploymentPolicyConfig' {stagingDistributionDnsNames} -> stagingDistributionDnsNames) (\s@ContinuousDeploymentPolicyConfig' {} a -> s {stagingDistributionDnsNames = a} :: ContinuousDeploymentPolicyConfig)

-- | A Boolean that indicates whether this continuous deployment policy is
-- enabled (in effect). When this value is @true@, this policy is enabled
-- and in effect. When this value is @false@, this policy is not enabled
-- and has no effect.
continuousDeploymentPolicyConfig_enabled :: Lens.Lens' ContinuousDeploymentPolicyConfig Prelude.Bool
continuousDeploymentPolicyConfig_enabled = Lens.lens (\ContinuousDeploymentPolicyConfig' {enabled} -> enabled) (\s@ContinuousDeploymentPolicyConfig' {} a -> s {enabled = a} :: ContinuousDeploymentPolicyConfig)

instance
  Data.FromXML
    ContinuousDeploymentPolicyConfig
  where
  parseXML x =
    ContinuousDeploymentPolicyConfig'
      Prelude.<$> (x Data..@? "TrafficConfig")
      Prelude.<*> (x Data..@ "StagingDistributionDnsNames")
      Prelude.<*> (x Data..@ "Enabled")

instance
  Prelude.Hashable
    ContinuousDeploymentPolicyConfig
  where
  hashWithSalt
    _salt
    ContinuousDeploymentPolicyConfig' {..} =
      _salt
        `Prelude.hashWithSalt` trafficConfig
        `Prelude.hashWithSalt` stagingDistributionDnsNames
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    ContinuousDeploymentPolicyConfig
  where
  rnf ContinuousDeploymentPolicyConfig' {..} =
    Prelude.rnf trafficConfig
      `Prelude.seq` Prelude.rnf stagingDistributionDnsNames
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToXML ContinuousDeploymentPolicyConfig where
  toXML ContinuousDeploymentPolicyConfig' {..} =
    Prelude.mconcat
      [ "TrafficConfig" Data.@= trafficConfig,
        "StagingDistributionDnsNames"
          Data.@= stagingDistributionDnsNames,
        "Enabled" Data.@= enabled
      ]

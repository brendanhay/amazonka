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
-- Module      : Amazonka.Discovery.Types.Ec2RecommendationsExportPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.Ec2RecommendationsExportPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.ReservedInstanceOptions
import Amazonka.Discovery.Types.Tenancy
import Amazonka.Discovery.Types.UsageMetricBasis
import qualified Amazonka.Prelude as Prelude

-- | Indicates that the exported data must include EC2 instance type matches
-- for on-premises servers that are discovered through Amazon Web Services
-- Application Discovery Service.
--
-- /See:/ 'newEc2RecommendationsExportPreferences' smart constructor.
data Ec2RecommendationsExportPreferences = Ec2RecommendationsExportPreferences'
  { -- | The recommended EC2 instance type that matches the CPU usage metric of
    -- server performance data.
    cpuPerformanceMetricBasis :: Prelude.Maybe UsageMetricBasis,
    -- | If set to true, the export
    -- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html#API_StartExportTask_RequestSyntax preferences>
    -- is set to @Ec2RecommendationsExportPreferences@.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An array of instance types to exclude from recommendations.
    excludedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The target Amazon Web Services Region for the recommendations. You can
    -- use any of the Region codes available for the chosen service, as listed
    -- in
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Amazon Web Services service endpoints>
    -- in the /Amazon Web Services General Reference/.
    preferredRegion :: Prelude.Maybe Prelude.Text,
    -- | The recommended EC2 instance type that matches the Memory usage metric
    -- of server performance data.
    ramPerformanceMetricBasis :: Prelude.Maybe UsageMetricBasis,
    -- | The contract type for a reserved instance. If blank, we assume an
    -- On-Demand instance is preferred.
    reservedInstanceOptions :: Prelude.Maybe ReservedInstanceOptions,
    -- | The target tenancy to use for your recommended EC2 instances.
    tenancy :: Prelude.Maybe Tenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2RecommendationsExportPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuPerformanceMetricBasis', 'ec2RecommendationsExportPreferences_cpuPerformanceMetricBasis' - The recommended EC2 instance type that matches the CPU usage metric of
-- server performance data.
--
-- 'enabled', 'ec2RecommendationsExportPreferences_enabled' - If set to true, the export
-- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html#API_StartExportTask_RequestSyntax preferences>
-- is set to @Ec2RecommendationsExportPreferences@.
--
-- 'excludedInstanceTypes', 'ec2RecommendationsExportPreferences_excludedInstanceTypes' - An array of instance types to exclude from recommendations.
--
-- 'preferredRegion', 'ec2RecommendationsExportPreferences_preferredRegion' - The target Amazon Web Services Region for the recommendations. You can
-- use any of the Region codes available for the chosen service, as listed
-- in
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Amazon Web Services service endpoints>
-- in the /Amazon Web Services General Reference/.
--
-- 'ramPerformanceMetricBasis', 'ec2RecommendationsExportPreferences_ramPerformanceMetricBasis' - The recommended EC2 instance type that matches the Memory usage metric
-- of server performance data.
--
-- 'reservedInstanceOptions', 'ec2RecommendationsExportPreferences_reservedInstanceOptions' - The contract type for a reserved instance. If blank, we assume an
-- On-Demand instance is preferred.
--
-- 'tenancy', 'ec2RecommendationsExportPreferences_tenancy' - The target tenancy to use for your recommended EC2 instances.
newEc2RecommendationsExportPreferences ::
  Ec2RecommendationsExportPreferences
newEc2RecommendationsExportPreferences =
  Ec2RecommendationsExportPreferences'
    { cpuPerformanceMetricBasis =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      excludedInstanceTypes =
        Prelude.Nothing,
      preferredRegion = Prelude.Nothing,
      ramPerformanceMetricBasis =
        Prelude.Nothing,
      reservedInstanceOptions =
        Prelude.Nothing,
      tenancy = Prelude.Nothing
    }

-- | The recommended EC2 instance type that matches the CPU usage metric of
-- server performance data.
ec2RecommendationsExportPreferences_cpuPerformanceMetricBasis :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe UsageMetricBasis)
ec2RecommendationsExportPreferences_cpuPerformanceMetricBasis = Lens.lens (\Ec2RecommendationsExportPreferences' {cpuPerformanceMetricBasis} -> cpuPerformanceMetricBasis) (\s@Ec2RecommendationsExportPreferences' {} a -> s {cpuPerformanceMetricBasis = a} :: Ec2RecommendationsExportPreferences)

-- | If set to true, the export
-- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html#API_StartExportTask_RequestSyntax preferences>
-- is set to @Ec2RecommendationsExportPreferences@.
ec2RecommendationsExportPreferences_enabled :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe Prelude.Bool)
ec2RecommendationsExportPreferences_enabled = Lens.lens (\Ec2RecommendationsExportPreferences' {enabled} -> enabled) (\s@Ec2RecommendationsExportPreferences' {} a -> s {enabled = a} :: Ec2RecommendationsExportPreferences)

-- | An array of instance types to exclude from recommendations.
ec2RecommendationsExportPreferences_excludedInstanceTypes :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe [Prelude.Text])
ec2RecommendationsExportPreferences_excludedInstanceTypes = Lens.lens (\Ec2RecommendationsExportPreferences' {excludedInstanceTypes} -> excludedInstanceTypes) (\s@Ec2RecommendationsExportPreferences' {} a -> s {excludedInstanceTypes = a} :: Ec2RecommendationsExportPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The target Amazon Web Services Region for the recommendations. You can
-- use any of the Region codes available for the chosen service, as listed
-- in
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Amazon Web Services service endpoints>
-- in the /Amazon Web Services General Reference/.
ec2RecommendationsExportPreferences_preferredRegion :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe Prelude.Text)
ec2RecommendationsExportPreferences_preferredRegion = Lens.lens (\Ec2RecommendationsExportPreferences' {preferredRegion} -> preferredRegion) (\s@Ec2RecommendationsExportPreferences' {} a -> s {preferredRegion = a} :: Ec2RecommendationsExportPreferences)

-- | The recommended EC2 instance type that matches the Memory usage metric
-- of server performance data.
ec2RecommendationsExportPreferences_ramPerformanceMetricBasis :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe UsageMetricBasis)
ec2RecommendationsExportPreferences_ramPerformanceMetricBasis = Lens.lens (\Ec2RecommendationsExportPreferences' {ramPerformanceMetricBasis} -> ramPerformanceMetricBasis) (\s@Ec2RecommendationsExportPreferences' {} a -> s {ramPerformanceMetricBasis = a} :: Ec2RecommendationsExportPreferences)

-- | The contract type for a reserved instance. If blank, we assume an
-- On-Demand instance is preferred.
ec2RecommendationsExportPreferences_reservedInstanceOptions :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe ReservedInstanceOptions)
ec2RecommendationsExportPreferences_reservedInstanceOptions = Lens.lens (\Ec2RecommendationsExportPreferences' {reservedInstanceOptions} -> reservedInstanceOptions) (\s@Ec2RecommendationsExportPreferences' {} a -> s {reservedInstanceOptions = a} :: Ec2RecommendationsExportPreferences)

-- | The target tenancy to use for your recommended EC2 instances.
ec2RecommendationsExportPreferences_tenancy :: Lens.Lens' Ec2RecommendationsExportPreferences (Prelude.Maybe Tenancy)
ec2RecommendationsExportPreferences_tenancy = Lens.lens (\Ec2RecommendationsExportPreferences' {tenancy} -> tenancy) (\s@Ec2RecommendationsExportPreferences' {} a -> s {tenancy = a} :: Ec2RecommendationsExportPreferences)

instance
  Prelude.Hashable
    Ec2RecommendationsExportPreferences
  where
  hashWithSalt
    _salt
    Ec2RecommendationsExportPreferences' {..} =
      _salt
        `Prelude.hashWithSalt` cpuPerformanceMetricBasis
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` excludedInstanceTypes
        `Prelude.hashWithSalt` preferredRegion
        `Prelude.hashWithSalt` ramPerformanceMetricBasis
        `Prelude.hashWithSalt` reservedInstanceOptions
        `Prelude.hashWithSalt` tenancy

instance
  Prelude.NFData
    Ec2RecommendationsExportPreferences
  where
  rnf Ec2RecommendationsExportPreferences' {..} =
    Prelude.rnf cpuPerformanceMetricBasis
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf excludedInstanceTypes
      `Prelude.seq` Prelude.rnf preferredRegion
      `Prelude.seq` Prelude.rnf ramPerformanceMetricBasis
      `Prelude.seq` Prelude.rnf reservedInstanceOptions
      `Prelude.seq` Prelude.rnf tenancy

instance
  Data.ToJSON
    Ec2RecommendationsExportPreferences
  where
  toJSON Ec2RecommendationsExportPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpuPerformanceMetricBasis" Data..=)
              Prelude.<$> cpuPerformanceMetricBasis,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("excludedInstanceTypes" Data..=)
              Prelude.<$> excludedInstanceTypes,
            ("preferredRegion" Data..=)
              Prelude.<$> preferredRegion,
            ("ramPerformanceMetricBasis" Data..=)
              Prelude.<$> ramPerformanceMetricBasis,
            ("reservedInstanceOptions" Data..=)
              Prelude.<$> reservedInstanceOptions,
            ("tenancy" Data..=) Prelude.<$> tenancy
          ]
      )

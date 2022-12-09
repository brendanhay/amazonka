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
-- Module      : Amazonka.ResilienceHub.Types.ConfigRecommendation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ConfigRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.ConfigRecommendationOptimizationType
import Amazonka.ResilienceHub.Types.Cost
import Amazonka.ResilienceHub.Types.DisruptionCompliance
import Amazonka.ResilienceHub.Types.DisruptionType
import Amazonka.ResilienceHub.Types.HaArchitecture
import Amazonka.ResilienceHub.Types.RecommendationDisruptionCompliance

-- | Defines a configuration recommendation.
--
-- /See:/ 'newConfigRecommendation' smart constructor.
data ConfigRecommendation = ConfigRecommendation'
  { -- | The application component name.
    appComponentName :: Prelude.Maybe Prelude.Text,
    -- | The current compliance against the resiliency policy before applying the
    -- configuration change.
    compliance :: Prelude.Maybe (Prelude.HashMap DisruptionType DisruptionCompliance),
    -- | The cost for the application.
    cost :: Prelude.Maybe Cost,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The architecture type.
    haArchitecture :: Prelude.Maybe HaArchitecture,
    -- | The expected compliance against the resiliency policy after applying the
    -- configuration change.
    recommendationCompliance :: Prelude.Maybe (Prelude.HashMap DisruptionType RecommendationDisruptionCompliance),
    -- | List of the suggested configuration changes.
    suggestedChanges :: Prelude.Maybe [Prelude.Text],
    -- | The name of the recommendation configuration.
    name :: Prelude.Text,
    -- | The type of optimization.
    optimizationType :: ConfigRecommendationOptimizationType,
    -- | The reference identifier for the recommendation configuration.
    referenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponentName', 'configRecommendation_appComponentName' - The application component name.
--
-- 'compliance', 'configRecommendation_compliance' - The current compliance against the resiliency policy before applying the
-- configuration change.
--
-- 'cost', 'configRecommendation_cost' - The cost for the application.
--
-- 'description', 'configRecommendation_description' - The optional description for an app.
--
-- 'haArchitecture', 'configRecommendation_haArchitecture' - The architecture type.
--
-- 'recommendationCompliance', 'configRecommendation_recommendationCompliance' - The expected compliance against the resiliency policy after applying the
-- configuration change.
--
-- 'suggestedChanges', 'configRecommendation_suggestedChanges' - List of the suggested configuration changes.
--
-- 'name', 'configRecommendation_name' - The name of the recommendation configuration.
--
-- 'optimizationType', 'configRecommendation_optimizationType' - The type of optimization.
--
-- 'referenceId', 'configRecommendation_referenceId' - The reference identifier for the recommendation configuration.
newConfigRecommendation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'optimizationType'
  ConfigRecommendationOptimizationType ->
  -- | 'referenceId'
  Prelude.Text ->
  ConfigRecommendation
newConfigRecommendation
  pName_
  pOptimizationType_
  pReferenceId_ =
    ConfigRecommendation'
      { appComponentName =
          Prelude.Nothing,
        compliance = Prelude.Nothing,
        cost = Prelude.Nothing,
        description = Prelude.Nothing,
        haArchitecture = Prelude.Nothing,
        recommendationCompliance = Prelude.Nothing,
        suggestedChanges = Prelude.Nothing,
        name = pName_,
        optimizationType = pOptimizationType_,
        referenceId = pReferenceId_
      }

-- | The application component name.
configRecommendation_appComponentName :: Lens.Lens' ConfigRecommendation (Prelude.Maybe Prelude.Text)
configRecommendation_appComponentName = Lens.lens (\ConfigRecommendation' {appComponentName} -> appComponentName) (\s@ConfigRecommendation' {} a -> s {appComponentName = a} :: ConfigRecommendation)

-- | The current compliance against the resiliency policy before applying the
-- configuration change.
configRecommendation_compliance :: Lens.Lens' ConfigRecommendation (Prelude.Maybe (Prelude.HashMap DisruptionType DisruptionCompliance))
configRecommendation_compliance = Lens.lens (\ConfigRecommendation' {compliance} -> compliance) (\s@ConfigRecommendation' {} a -> s {compliance = a} :: ConfigRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The cost for the application.
configRecommendation_cost :: Lens.Lens' ConfigRecommendation (Prelude.Maybe Cost)
configRecommendation_cost = Lens.lens (\ConfigRecommendation' {cost} -> cost) (\s@ConfigRecommendation' {} a -> s {cost = a} :: ConfigRecommendation)

-- | The optional description for an app.
configRecommendation_description :: Lens.Lens' ConfigRecommendation (Prelude.Maybe Prelude.Text)
configRecommendation_description = Lens.lens (\ConfigRecommendation' {description} -> description) (\s@ConfigRecommendation' {} a -> s {description = a} :: ConfigRecommendation)

-- | The architecture type.
configRecommendation_haArchitecture :: Lens.Lens' ConfigRecommendation (Prelude.Maybe HaArchitecture)
configRecommendation_haArchitecture = Lens.lens (\ConfigRecommendation' {haArchitecture} -> haArchitecture) (\s@ConfigRecommendation' {} a -> s {haArchitecture = a} :: ConfigRecommendation)

-- | The expected compliance against the resiliency policy after applying the
-- configuration change.
configRecommendation_recommendationCompliance :: Lens.Lens' ConfigRecommendation (Prelude.Maybe (Prelude.HashMap DisruptionType RecommendationDisruptionCompliance))
configRecommendation_recommendationCompliance = Lens.lens (\ConfigRecommendation' {recommendationCompliance} -> recommendationCompliance) (\s@ConfigRecommendation' {} a -> s {recommendationCompliance = a} :: ConfigRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | List of the suggested configuration changes.
configRecommendation_suggestedChanges :: Lens.Lens' ConfigRecommendation (Prelude.Maybe [Prelude.Text])
configRecommendation_suggestedChanges = Lens.lens (\ConfigRecommendation' {suggestedChanges} -> suggestedChanges) (\s@ConfigRecommendation' {} a -> s {suggestedChanges = a} :: ConfigRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the recommendation configuration.
configRecommendation_name :: Lens.Lens' ConfigRecommendation Prelude.Text
configRecommendation_name = Lens.lens (\ConfigRecommendation' {name} -> name) (\s@ConfigRecommendation' {} a -> s {name = a} :: ConfigRecommendation)

-- | The type of optimization.
configRecommendation_optimizationType :: Lens.Lens' ConfigRecommendation ConfigRecommendationOptimizationType
configRecommendation_optimizationType = Lens.lens (\ConfigRecommendation' {optimizationType} -> optimizationType) (\s@ConfigRecommendation' {} a -> s {optimizationType = a} :: ConfigRecommendation)

-- | The reference identifier for the recommendation configuration.
configRecommendation_referenceId :: Lens.Lens' ConfigRecommendation Prelude.Text
configRecommendation_referenceId = Lens.lens (\ConfigRecommendation' {referenceId} -> referenceId) (\s@ConfigRecommendation' {} a -> s {referenceId = a} :: ConfigRecommendation)

instance Data.FromJSON ConfigRecommendation where
  parseJSON =
    Data.withObject
      "ConfigRecommendation"
      ( \x ->
          ConfigRecommendation'
            Prelude.<$> (x Data..:? "appComponentName")
            Prelude.<*> (x Data..:? "compliance" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "cost")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "haArchitecture")
            Prelude.<*> ( x Data..:? "recommendationCompliance"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "suggestedChanges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "optimizationType")
            Prelude.<*> (x Data..: "referenceId")
      )

instance Prelude.Hashable ConfigRecommendation where
  hashWithSalt _salt ConfigRecommendation' {..} =
    _salt `Prelude.hashWithSalt` appComponentName
      `Prelude.hashWithSalt` compliance
      `Prelude.hashWithSalt` cost
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` haArchitecture
      `Prelude.hashWithSalt` recommendationCompliance
      `Prelude.hashWithSalt` suggestedChanges
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` optimizationType
      `Prelude.hashWithSalt` referenceId

instance Prelude.NFData ConfigRecommendation where
  rnf ConfigRecommendation' {..} =
    Prelude.rnf appComponentName
      `Prelude.seq` Prelude.rnf compliance
      `Prelude.seq` Prelude.rnf cost
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf haArchitecture
      `Prelude.seq` Prelude.rnf recommendationCompliance
      `Prelude.seq` Prelude.rnf suggestedChanges
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf optimizationType
      `Prelude.seq` Prelude.rnf referenceId

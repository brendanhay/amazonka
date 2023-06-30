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
-- Module      : Amazonka.ResilienceHub.Types.ComponentRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ComponentRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.ConfigRecommendation
import Amazonka.ResilienceHub.Types.RecommendationComplianceStatus

-- | Defines recommendations for a Resilience Hub application component,
-- returned as an object. This object contains component names,
-- configuration recommendations, and recommendation statuses.
--
-- /See:/ 'newComponentRecommendation' smart constructor.
data ComponentRecommendation = ComponentRecommendation'
  { -- | The name of the application component.
    appComponentName :: Prelude.Text,
    -- | The list of recommendations.
    configRecommendations :: [ConfigRecommendation],
    -- | The recommendation status.
    recommendationStatus :: RecommendationComplianceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponentName', 'componentRecommendation_appComponentName' - The name of the application component.
--
-- 'configRecommendations', 'componentRecommendation_configRecommendations' - The list of recommendations.
--
-- 'recommendationStatus', 'componentRecommendation_recommendationStatus' - The recommendation status.
newComponentRecommendation ::
  -- | 'appComponentName'
  Prelude.Text ->
  -- | 'recommendationStatus'
  RecommendationComplianceStatus ->
  ComponentRecommendation
newComponentRecommendation
  pAppComponentName_
  pRecommendationStatus_ =
    ComponentRecommendation'
      { appComponentName =
          pAppComponentName_,
        configRecommendations = Prelude.mempty,
        recommendationStatus = pRecommendationStatus_
      }

-- | The name of the application component.
componentRecommendation_appComponentName :: Lens.Lens' ComponentRecommendation Prelude.Text
componentRecommendation_appComponentName = Lens.lens (\ComponentRecommendation' {appComponentName} -> appComponentName) (\s@ComponentRecommendation' {} a -> s {appComponentName = a} :: ComponentRecommendation)

-- | The list of recommendations.
componentRecommendation_configRecommendations :: Lens.Lens' ComponentRecommendation [ConfigRecommendation]
componentRecommendation_configRecommendations = Lens.lens (\ComponentRecommendation' {configRecommendations} -> configRecommendations) (\s@ComponentRecommendation' {} a -> s {configRecommendations = a} :: ComponentRecommendation) Prelude.. Lens.coerced

-- | The recommendation status.
componentRecommendation_recommendationStatus :: Lens.Lens' ComponentRecommendation RecommendationComplianceStatus
componentRecommendation_recommendationStatus = Lens.lens (\ComponentRecommendation' {recommendationStatus} -> recommendationStatus) (\s@ComponentRecommendation' {} a -> s {recommendationStatus = a} :: ComponentRecommendation)

instance Data.FromJSON ComponentRecommendation where
  parseJSON =
    Data.withObject
      "ComponentRecommendation"
      ( \x ->
          ComponentRecommendation'
            Prelude.<$> (x Data..: "appComponentName")
            Prelude.<*> ( x
                            Data..:? "configRecommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "recommendationStatus")
      )

instance Prelude.Hashable ComponentRecommendation where
  hashWithSalt _salt ComponentRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` appComponentName
      `Prelude.hashWithSalt` configRecommendations
      `Prelude.hashWithSalt` recommendationStatus

instance Prelude.NFData ComponentRecommendation where
  rnf ComponentRecommendation' {..} =
    Prelude.rnf appComponentName
      `Prelude.seq` Prelude.rnf configRecommendations
      `Prelude.seq` Prelude.rnf recommendationStatus

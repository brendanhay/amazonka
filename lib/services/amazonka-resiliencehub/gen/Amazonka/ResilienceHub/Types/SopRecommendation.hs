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
-- Module      : Amazonka.ResilienceHub.Types.SopRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.SopRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.RecommendationItem
import Amazonka.ResilienceHub.Types.SopServiceType

-- | Defines a standard operating procedure (SOP) recommendation.
--
-- /See:/ 'newSopRecommendation' smart constructor.
data SopRecommendation = SopRecommendation'
  { -- | The application component name.
    appComponentName :: Prelude.Maybe Prelude.Text,
    -- | The description of the SOP recommendation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The recommendation items.
    items :: Prelude.Maybe [RecommendationItem],
    -- | The name of the SOP recommendation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The prerequisite for the SOP recommendation.
    prerequisite :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the SOP recommendation.
    recommendationId :: Prelude.Text,
    -- | The reference identifier for the SOP recommendation.
    referenceId :: Prelude.Text,
    -- | The service type.
    serviceType :: SopServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SopRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponentName', 'sopRecommendation_appComponentName' - The application component name.
--
-- 'description', 'sopRecommendation_description' - The description of the SOP recommendation.
--
-- 'items', 'sopRecommendation_items' - The recommendation items.
--
-- 'name', 'sopRecommendation_name' - The name of the SOP recommendation.
--
-- 'prerequisite', 'sopRecommendation_prerequisite' - The prerequisite for the SOP recommendation.
--
-- 'recommendationId', 'sopRecommendation_recommendationId' - Identifier for the SOP recommendation.
--
-- 'referenceId', 'sopRecommendation_referenceId' - The reference identifier for the SOP recommendation.
--
-- 'serviceType', 'sopRecommendation_serviceType' - The service type.
newSopRecommendation ::
  -- | 'recommendationId'
  Prelude.Text ->
  -- | 'referenceId'
  Prelude.Text ->
  -- | 'serviceType'
  SopServiceType ->
  SopRecommendation
newSopRecommendation
  pRecommendationId_
  pReferenceId_
  pServiceType_ =
    SopRecommendation'
      { appComponentName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        items = Prelude.Nothing,
        name = Prelude.Nothing,
        prerequisite = Prelude.Nothing,
        recommendationId = pRecommendationId_,
        referenceId = pReferenceId_,
        serviceType = pServiceType_
      }

-- | The application component name.
sopRecommendation_appComponentName :: Lens.Lens' SopRecommendation (Prelude.Maybe Prelude.Text)
sopRecommendation_appComponentName = Lens.lens (\SopRecommendation' {appComponentName} -> appComponentName) (\s@SopRecommendation' {} a -> s {appComponentName = a} :: SopRecommendation)

-- | The description of the SOP recommendation.
sopRecommendation_description :: Lens.Lens' SopRecommendation (Prelude.Maybe Prelude.Text)
sopRecommendation_description = Lens.lens (\SopRecommendation' {description} -> description) (\s@SopRecommendation' {} a -> s {description = a} :: SopRecommendation)

-- | The recommendation items.
sopRecommendation_items :: Lens.Lens' SopRecommendation (Prelude.Maybe [RecommendationItem])
sopRecommendation_items = Lens.lens (\SopRecommendation' {items} -> items) (\s@SopRecommendation' {} a -> s {items = a} :: SopRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the SOP recommendation.
sopRecommendation_name :: Lens.Lens' SopRecommendation (Prelude.Maybe Prelude.Text)
sopRecommendation_name = Lens.lens (\SopRecommendation' {name} -> name) (\s@SopRecommendation' {} a -> s {name = a} :: SopRecommendation)

-- | The prerequisite for the SOP recommendation.
sopRecommendation_prerequisite :: Lens.Lens' SopRecommendation (Prelude.Maybe Prelude.Text)
sopRecommendation_prerequisite = Lens.lens (\SopRecommendation' {prerequisite} -> prerequisite) (\s@SopRecommendation' {} a -> s {prerequisite = a} :: SopRecommendation)

-- | Identifier for the SOP recommendation.
sopRecommendation_recommendationId :: Lens.Lens' SopRecommendation Prelude.Text
sopRecommendation_recommendationId = Lens.lens (\SopRecommendation' {recommendationId} -> recommendationId) (\s@SopRecommendation' {} a -> s {recommendationId = a} :: SopRecommendation)

-- | The reference identifier for the SOP recommendation.
sopRecommendation_referenceId :: Lens.Lens' SopRecommendation Prelude.Text
sopRecommendation_referenceId = Lens.lens (\SopRecommendation' {referenceId} -> referenceId) (\s@SopRecommendation' {} a -> s {referenceId = a} :: SopRecommendation)

-- | The service type.
sopRecommendation_serviceType :: Lens.Lens' SopRecommendation SopServiceType
sopRecommendation_serviceType = Lens.lens (\SopRecommendation' {serviceType} -> serviceType) (\s@SopRecommendation' {} a -> s {serviceType = a} :: SopRecommendation)

instance Data.FromJSON SopRecommendation where
  parseJSON =
    Data.withObject
      "SopRecommendation"
      ( \x ->
          SopRecommendation'
            Prelude.<$> (x Data..:? "appComponentName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "items" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "prerequisite")
            Prelude.<*> (x Data..: "recommendationId")
            Prelude.<*> (x Data..: "referenceId")
            Prelude.<*> (x Data..: "serviceType")
      )

instance Prelude.Hashable SopRecommendation where
  hashWithSalt _salt SopRecommendation' {..} =
    _salt `Prelude.hashWithSalt` appComponentName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` prerequisite
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` serviceType

instance Prelude.NFData SopRecommendation where
  rnf SopRecommendation' {..} =
    Prelude.rnf appComponentName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf prerequisite
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf serviceType

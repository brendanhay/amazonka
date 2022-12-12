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
-- Module      : Amazonka.ResilienceHub.Types.TestRecommendation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.TestRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.RecommendationItem
import Amazonka.ResilienceHub.Types.TestRisk
import Amazonka.ResilienceHub.Types.TestType

-- | Defines a test recommendation.
--
-- /See:/ 'newTestRecommendation' smart constructor.
data TestRecommendation = TestRecommendation'
  { -- | The name of the application component.
    appComponentName :: Prelude.Maybe Prelude.Text,
    -- | A list of recommended alarms that are used in the test and must be
    -- exported before or with the test.
    dependsOnAlarms :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The description for the test recommendation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The intent of the test recommendation.
    intent :: Prelude.Maybe Prelude.Text,
    -- | The test recommendation items.
    items :: Prelude.Maybe [RecommendationItem],
    -- | The name of the test recommendation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The prerequisite of the test recommendation.
    prerequisite :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the test recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The level of risk for this test recommendation.
    risk :: Prelude.Maybe TestRisk,
    -- | The type of test recommendation.
    type' :: Prelude.Maybe TestType,
    -- | The reference identifier for the test recommendation.
    referenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponentName', 'testRecommendation_appComponentName' - The name of the application component.
--
-- 'dependsOnAlarms', 'testRecommendation_dependsOnAlarms' - A list of recommended alarms that are used in the test and must be
-- exported before or with the test.
--
-- 'description', 'testRecommendation_description' - The description for the test recommendation.
--
-- 'intent', 'testRecommendation_intent' - The intent of the test recommendation.
--
-- 'items', 'testRecommendation_items' - The test recommendation items.
--
-- 'name', 'testRecommendation_name' - The name of the test recommendation.
--
-- 'prerequisite', 'testRecommendation_prerequisite' - The prerequisite of the test recommendation.
--
-- 'recommendationId', 'testRecommendation_recommendationId' - Identifier for the test recommendation.
--
-- 'risk', 'testRecommendation_risk' - The level of risk for this test recommendation.
--
-- 'type'', 'testRecommendation_type' - The type of test recommendation.
--
-- 'referenceId', 'testRecommendation_referenceId' - The reference identifier for the test recommendation.
newTestRecommendation ::
  -- | 'referenceId'
  Prelude.Text ->
  TestRecommendation
newTestRecommendation pReferenceId_ =
  TestRecommendation'
    { appComponentName =
        Prelude.Nothing,
      dependsOnAlarms = Prelude.Nothing,
      description = Prelude.Nothing,
      intent = Prelude.Nothing,
      items = Prelude.Nothing,
      name = Prelude.Nothing,
      prerequisite = Prelude.Nothing,
      recommendationId = Prelude.Nothing,
      risk = Prelude.Nothing,
      type' = Prelude.Nothing,
      referenceId = pReferenceId_
    }

-- | The name of the application component.
testRecommendation_appComponentName :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_appComponentName = Lens.lens (\TestRecommendation' {appComponentName} -> appComponentName) (\s@TestRecommendation' {} a -> s {appComponentName = a} :: TestRecommendation)

-- | A list of recommended alarms that are used in the test and must be
-- exported before or with the test.
testRecommendation_dependsOnAlarms :: Lens.Lens' TestRecommendation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
testRecommendation_dependsOnAlarms = Lens.lens (\TestRecommendation' {dependsOnAlarms} -> dependsOnAlarms) (\s@TestRecommendation' {} a -> s {dependsOnAlarms = a} :: TestRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The description for the test recommendation.
testRecommendation_description :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_description = Lens.lens (\TestRecommendation' {description} -> description) (\s@TestRecommendation' {} a -> s {description = a} :: TestRecommendation)

-- | The intent of the test recommendation.
testRecommendation_intent :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_intent = Lens.lens (\TestRecommendation' {intent} -> intent) (\s@TestRecommendation' {} a -> s {intent = a} :: TestRecommendation)

-- | The test recommendation items.
testRecommendation_items :: Lens.Lens' TestRecommendation (Prelude.Maybe [RecommendationItem])
testRecommendation_items = Lens.lens (\TestRecommendation' {items} -> items) (\s@TestRecommendation' {} a -> s {items = a} :: TestRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the test recommendation.
testRecommendation_name :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_name = Lens.lens (\TestRecommendation' {name} -> name) (\s@TestRecommendation' {} a -> s {name = a} :: TestRecommendation)

-- | The prerequisite of the test recommendation.
testRecommendation_prerequisite :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_prerequisite = Lens.lens (\TestRecommendation' {prerequisite} -> prerequisite) (\s@TestRecommendation' {} a -> s {prerequisite = a} :: TestRecommendation)

-- | Identifier for the test recommendation.
testRecommendation_recommendationId :: Lens.Lens' TestRecommendation (Prelude.Maybe Prelude.Text)
testRecommendation_recommendationId = Lens.lens (\TestRecommendation' {recommendationId} -> recommendationId) (\s@TestRecommendation' {} a -> s {recommendationId = a} :: TestRecommendation)

-- | The level of risk for this test recommendation.
testRecommendation_risk :: Lens.Lens' TestRecommendation (Prelude.Maybe TestRisk)
testRecommendation_risk = Lens.lens (\TestRecommendation' {risk} -> risk) (\s@TestRecommendation' {} a -> s {risk = a} :: TestRecommendation)

-- | The type of test recommendation.
testRecommendation_type :: Lens.Lens' TestRecommendation (Prelude.Maybe TestType)
testRecommendation_type = Lens.lens (\TestRecommendation' {type'} -> type') (\s@TestRecommendation' {} a -> s {type' = a} :: TestRecommendation)

-- | The reference identifier for the test recommendation.
testRecommendation_referenceId :: Lens.Lens' TestRecommendation Prelude.Text
testRecommendation_referenceId = Lens.lens (\TestRecommendation' {referenceId} -> referenceId) (\s@TestRecommendation' {} a -> s {referenceId = a} :: TestRecommendation)

instance Data.FromJSON TestRecommendation where
  parseJSON =
    Data.withObject
      "TestRecommendation"
      ( \x ->
          TestRecommendation'
            Prelude.<$> (x Data..:? "appComponentName")
            Prelude.<*> (x Data..:? "dependsOnAlarms")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "intent")
            Prelude.<*> (x Data..:? "items" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "prerequisite")
            Prelude.<*> (x Data..:? "recommendationId")
            Prelude.<*> (x Data..:? "risk")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "referenceId")
      )

instance Prelude.Hashable TestRecommendation where
  hashWithSalt _salt TestRecommendation' {..} =
    _salt `Prelude.hashWithSalt` appComponentName
      `Prelude.hashWithSalt` dependsOnAlarms
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` intent
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` prerequisite
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` risk
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` referenceId

instance Prelude.NFData TestRecommendation where
  rnf TestRecommendation' {..} =
    Prelude.rnf appComponentName
      `Prelude.seq` Prelude.rnf dependsOnAlarms
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf intent
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf prerequisite
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf risk
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf referenceId

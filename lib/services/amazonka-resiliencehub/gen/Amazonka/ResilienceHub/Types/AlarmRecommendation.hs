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
-- Module      : Amazonka.ResilienceHub.Types.AlarmRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AlarmRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AlarmType
import Amazonka.ResilienceHub.Types.RecommendationItem

-- | Defines a recommendation for a CloudWatch alarm.
--
-- /See:/ 'newAlarmRecommendation' smart constructor.
data AlarmRecommendation = AlarmRecommendation'
  { -- | The application component for the CloudWatch alarm recommendation.
    appComponentName :: Prelude.Maybe Prelude.Text,
    -- | The description of the recommendation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of CloudWatch alarm recommendations.
    items :: Prelude.Maybe [RecommendationItem],
    -- | The prerequisite for the alarm recommendation.
    prerequisite :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm recommendation.
    name :: Prelude.Text,
    -- | The identifier of the alarm recommendation.
    recommendationId :: Prelude.Text,
    -- | The reference identifier of the alarm recommendation.
    referenceId :: Prelude.Text,
    -- | The type of alarm recommendation.
    type' :: AlarmType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponentName', 'alarmRecommendation_appComponentName' - The application component for the CloudWatch alarm recommendation.
--
-- 'description', 'alarmRecommendation_description' - The description of the recommendation.
--
-- 'items', 'alarmRecommendation_items' - The list of CloudWatch alarm recommendations.
--
-- 'prerequisite', 'alarmRecommendation_prerequisite' - The prerequisite for the alarm recommendation.
--
-- 'name', 'alarmRecommendation_name' - The name of the alarm recommendation.
--
-- 'recommendationId', 'alarmRecommendation_recommendationId' - The identifier of the alarm recommendation.
--
-- 'referenceId', 'alarmRecommendation_referenceId' - The reference identifier of the alarm recommendation.
--
-- 'type'', 'alarmRecommendation_type' - The type of alarm recommendation.
newAlarmRecommendation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'recommendationId'
  Prelude.Text ->
  -- | 'referenceId'
  Prelude.Text ->
  -- | 'type''
  AlarmType ->
  AlarmRecommendation
newAlarmRecommendation
  pName_
  pRecommendationId_
  pReferenceId_
  pType_ =
    AlarmRecommendation'
      { appComponentName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        items = Prelude.Nothing,
        prerequisite = Prelude.Nothing,
        name = pName_,
        recommendationId = pRecommendationId_,
        referenceId = pReferenceId_,
        type' = pType_
      }

-- | The application component for the CloudWatch alarm recommendation.
alarmRecommendation_appComponentName :: Lens.Lens' AlarmRecommendation (Prelude.Maybe Prelude.Text)
alarmRecommendation_appComponentName = Lens.lens (\AlarmRecommendation' {appComponentName} -> appComponentName) (\s@AlarmRecommendation' {} a -> s {appComponentName = a} :: AlarmRecommendation)

-- | The description of the recommendation.
alarmRecommendation_description :: Lens.Lens' AlarmRecommendation (Prelude.Maybe Prelude.Text)
alarmRecommendation_description = Lens.lens (\AlarmRecommendation' {description} -> description) (\s@AlarmRecommendation' {} a -> s {description = a} :: AlarmRecommendation)

-- | The list of CloudWatch alarm recommendations.
alarmRecommendation_items :: Lens.Lens' AlarmRecommendation (Prelude.Maybe [RecommendationItem])
alarmRecommendation_items = Lens.lens (\AlarmRecommendation' {items} -> items) (\s@AlarmRecommendation' {} a -> s {items = a} :: AlarmRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The prerequisite for the alarm recommendation.
alarmRecommendation_prerequisite :: Lens.Lens' AlarmRecommendation (Prelude.Maybe Prelude.Text)
alarmRecommendation_prerequisite = Lens.lens (\AlarmRecommendation' {prerequisite} -> prerequisite) (\s@AlarmRecommendation' {} a -> s {prerequisite = a} :: AlarmRecommendation)

-- | The name of the alarm recommendation.
alarmRecommendation_name :: Lens.Lens' AlarmRecommendation Prelude.Text
alarmRecommendation_name = Lens.lens (\AlarmRecommendation' {name} -> name) (\s@AlarmRecommendation' {} a -> s {name = a} :: AlarmRecommendation)

-- | The identifier of the alarm recommendation.
alarmRecommendation_recommendationId :: Lens.Lens' AlarmRecommendation Prelude.Text
alarmRecommendation_recommendationId = Lens.lens (\AlarmRecommendation' {recommendationId} -> recommendationId) (\s@AlarmRecommendation' {} a -> s {recommendationId = a} :: AlarmRecommendation)

-- | The reference identifier of the alarm recommendation.
alarmRecommendation_referenceId :: Lens.Lens' AlarmRecommendation Prelude.Text
alarmRecommendation_referenceId = Lens.lens (\AlarmRecommendation' {referenceId} -> referenceId) (\s@AlarmRecommendation' {} a -> s {referenceId = a} :: AlarmRecommendation)

-- | The type of alarm recommendation.
alarmRecommendation_type :: Lens.Lens' AlarmRecommendation AlarmType
alarmRecommendation_type = Lens.lens (\AlarmRecommendation' {type'} -> type') (\s@AlarmRecommendation' {} a -> s {type' = a} :: AlarmRecommendation)

instance Data.FromJSON AlarmRecommendation where
  parseJSON =
    Data.withObject
      "AlarmRecommendation"
      ( \x ->
          AlarmRecommendation'
            Prelude.<$> (x Data..:? "appComponentName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "items" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "prerequisite")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "recommendationId")
            Prelude.<*> (x Data..: "referenceId")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AlarmRecommendation where
  hashWithSalt _salt AlarmRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` appComponentName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` prerequisite
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AlarmRecommendation where
  rnf AlarmRecommendation' {..} =
    Prelude.rnf appComponentName `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf items `Prelude.seq`
          Prelude.rnf prerequisite `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf recommendationId `Prelude.seq`
                Prelude.rnf referenceId `Prelude.seq`
                  Prelude.rnf type'

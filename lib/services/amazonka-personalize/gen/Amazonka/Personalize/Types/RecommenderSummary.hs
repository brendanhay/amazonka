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
-- Module      : Amazonka.Personalize.Types.RecommenderSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.RecommenderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.RecommenderConfig
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of the recommender.
--
-- /See:/ 'newRecommenderSummary' smart constructor.
data RecommenderSummary = RecommenderSummary'
  { -- | The name of the recommender.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the recommender was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the recommender.
    recommenderArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration details of the recommender.
    recommenderConfig :: Prelude.Maybe RecommenderConfig,
    -- | The status of the recommender. A recommender can be in one of the
    -- following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
    --     IN_PROGRESS > ACTIVE
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Domain dataset group that contains
    -- the recommender.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recipe (Domain dataset group use
    -- case) that the recommender was created for.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the recommender was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommenderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recommenderSummary_name' - The name of the recommender.
--
-- 'creationDateTime', 'recommenderSummary_creationDateTime' - The date and time (in Unix format) that the recommender was created.
--
-- 'recommenderArn', 'recommenderSummary_recommenderArn' - The Amazon Resource Name (ARN) of the recommender.
--
-- 'recommenderConfig', 'recommenderSummary_recommenderConfig' - The configuration details of the recommender.
--
-- 'status', 'recommenderSummary_status' - The status of the recommender. A recommender can be in one of the
-- following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'datasetGroupArn', 'recommenderSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the Domain dataset group that contains
-- the recommender.
--
-- 'recipeArn', 'recommenderSummary_recipeArn' - The Amazon Resource Name (ARN) of the recipe (Domain dataset group use
-- case) that the recommender was created for.
--
-- 'lastUpdatedDateTime', 'recommenderSummary_lastUpdatedDateTime' - The date and time (in Unix format) that the recommender was last
-- updated.
newRecommenderSummary ::
  RecommenderSummary
newRecommenderSummary =
  RecommenderSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      recommenderArn = Prelude.Nothing,
      recommenderConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the recommender.
recommenderSummary_name :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.Text)
recommenderSummary_name = Lens.lens (\RecommenderSummary' {name} -> name) (\s@RecommenderSummary' {} a -> s {name = a} :: RecommenderSummary)

-- | The date and time (in Unix format) that the recommender was created.
recommenderSummary_creationDateTime :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.UTCTime)
recommenderSummary_creationDateTime = Lens.lens (\RecommenderSummary' {creationDateTime} -> creationDateTime) (\s@RecommenderSummary' {} a -> s {creationDateTime = a} :: RecommenderSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the recommender.
recommenderSummary_recommenderArn :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.Text)
recommenderSummary_recommenderArn = Lens.lens (\RecommenderSummary' {recommenderArn} -> recommenderArn) (\s@RecommenderSummary' {} a -> s {recommenderArn = a} :: RecommenderSummary)

-- | The configuration details of the recommender.
recommenderSummary_recommenderConfig :: Lens.Lens' RecommenderSummary (Prelude.Maybe RecommenderConfig)
recommenderSummary_recommenderConfig = Lens.lens (\RecommenderSummary' {recommenderConfig} -> recommenderConfig) (\s@RecommenderSummary' {} a -> s {recommenderConfig = a} :: RecommenderSummary)

-- | The status of the recommender. A recommender can be in one of the
-- following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
recommenderSummary_status :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.Text)
recommenderSummary_status = Lens.lens (\RecommenderSummary' {status} -> status) (\s@RecommenderSummary' {} a -> s {status = a} :: RecommenderSummary)

-- | The Amazon Resource Name (ARN) of the Domain dataset group that contains
-- the recommender.
recommenderSummary_datasetGroupArn :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.Text)
recommenderSummary_datasetGroupArn = Lens.lens (\RecommenderSummary' {datasetGroupArn} -> datasetGroupArn) (\s@RecommenderSummary' {} a -> s {datasetGroupArn = a} :: RecommenderSummary)

-- | The Amazon Resource Name (ARN) of the recipe (Domain dataset group use
-- case) that the recommender was created for.
recommenderSummary_recipeArn :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.Text)
recommenderSummary_recipeArn = Lens.lens (\RecommenderSummary' {recipeArn} -> recipeArn) (\s@RecommenderSummary' {} a -> s {recipeArn = a} :: RecommenderSummary)

-- | The date and time (in Unix format) that the recommender was last
-- updated.
recommenderSummary_lastUpdatedDateTime :: Lens.Lens' RecommenderSummary (Prelude.Maybe Prelude.UTCTime)
recommenderSummary_lastUpdatedDateTime = Lens.lens (\RecommenderSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RecommenderSummary' {} a -> s {lastUpdatedDateTime = a} :: RecommenderSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON RecommenderSummary where
  parseJSON =
    Data.withObject
      "RecommenderSummary"
      ( \x ->
          RecommenderSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "recommenderArn")
            Prelude.<*> (x Data..:? "recommenderConfig")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "recipeArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable RecommenderSummary where
  hashWithSalt _salt RecommenderSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` recommenderArn
      `Prelude.hashWithSalt` recommenderConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData RecommenderSummary where
  rnf RecommenderSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf recommenderConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime

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
-- Module      : Amazonka.Personalize.Types.RecommenderUpdateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.RecommenderUpdateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.RecommenderConfig
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a recommender update. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeRecommender.html DescribeRecommender>
-- API.
--
-- /See:/ 'newRecommenderUpdateSummary' smart constructor.
data RecommenderUpdateSummary = RecommenderUpdateSummary'
  { -- | The date and time (in Unix format) that the recommender update was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If a recommender update fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the recommender update was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The configuration details of the recommender update.
    recommenderConfig :: Prelude.Maybe RecommenderConfig,
    -- | The status of the recommender update.
    --
    -- A recommender can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
    --     IN_PROGRESS > ACTIVE
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommenderUpdateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'recommenderUpdateSummary_creationDateTime' - The date and time (in Unix format) that the recommender update was
-- created.
--
-- 'failureReason', 'recommenderUpdateSummary_failureReason' - If a recommender update fails, the reason behind the failure.
--
-- 'lastUpdatedDateTime', 'recommenderUpdateSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the recommender update was last
-- updated.
--
-- 'recommenderConfig', 'recommenderUpdateSummary_recommenderConfig' - The configuration details of the recommender update.
--
-- 'status', 'recommenderUpdateSummary_status' - The status of the recommender update.
--
-- A recommender can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
newRecommenderUpdateSummary ::
  RecommenderUpdateSummary
newRecommenderUpdateSummary =
  RecommenderUpdateSummary'
    { creationDateTime =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      recommenderConfig = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix format) that the recommender update was
-- created.
recommenderUpdateSummary_creationDateTime :: Lens.Lens' RecommenderUpdateSummary (Prelude.Maybe Prelude.UTCTime)
recommenderUpdateSummary_creationDateTime = Lens.lens (\RecommenderUpdateSummary' {creationDateTime} -> creationDateTime) (\s@RecommenderUpdateSummary' {} a -> s {creationDateTime = a} :: RecommenderUpdateSummary) Prelude.. Lens.mapping Data._Time

-- | If a recommender update fails, the reason behind the failure.
recommenderUpdateSummary_failureReason :: Lens.Lens' RecommenderUpdateSummary (Prelude.Maybe Prelude.Text)
recommenderUpdateSummary_failureReason = Lens.lens (\RecommenderUpdateSummary' {failureReason} -> failureReason) (\s@RecommenderUpdateSummary' {} a -> s {failureReason = a} :: RecommenderUpdateSummary)

-- | The date and time (in Unix time) that the recommender update was last
-- updated.
recommenderUpdateSummary_lastUpdatedDateTime :: Lens.Lens' RecommenderUpdateSummary (Prelude.Maybe Prelude.UTCTime)
recommenderUpdateSummary_lastUpdatedDateTime = Lens.lens (\RecommenderUpdateSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RecommenderUpdateSummary' {} a -> s {lastUpdatedDateTime = a} :: RecommenderUpdateSummary) Prelude.. Lens.mapping Data._Time

-- | The configuration details of the recommender update.
recommenderUpdateSummary_recommenderConfig :: Lens.Lens' RecommenderUpdateSummary (Prelude.Maybe RecommenderConfig)
recommenderUpdateSummary_recommenderConfig = Lens.lens (\RecommenderUpdateSummary' {recommenderConfig} -> recommenderConfig) (\s@RecommenderUpdateSummary' {} a -> s {recommenderConfig = a} :: RecommenderUpdateSummary)

-- | The status of the recommender update.
--
-- A recommender can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
recommenderUpdateSummary_status :: Lens.Lens' RecommenderUpdateSummary (Prelude.Maybe Prelude.Text)
recommenderUpdateSummary_status = Lens.lens (\RecommenderUpdateSummary' {status} -> status) (\s@RecommenderUpdateSummary' {} a -> s {status = a} :: RecommenderUpdateSummary)

instance Data.FromJSON RecommenderUpdateSummary where
  parseJSON =
    Data.withObject
      "RecommenderUpdateSummary"
      ( \x ->
          RecommenderUpdateSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "recommenderConfig")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable RecommenderUpdateSummary where
  hashWithSalt _salt RecommenderUpdateSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` recommenderConfig
      `Prelude.hashWithSalt` status

instance Prelude.NFData RecommenderUpdateSummary where
  rnf RecommenderUpdateSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf recommenderConfig
      `Prelude.seq` Prelude.rnf status

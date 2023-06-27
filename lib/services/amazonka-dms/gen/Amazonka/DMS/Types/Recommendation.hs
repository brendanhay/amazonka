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
-- Module      : Amazonka.DMS.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RecommendationData
import Amazonka.DMS.Types.RecommendationSettings
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes a recommendation of a target engine.
--
-- A /recommendation/ is a set of possible Amazon Web Services target
-- engines that you can choose to migrate your source on-premises database.
-- In this set, Fleet Advisor suggests a single target engine as the right
-- sized migration destination. To determine this rightsized migration
-- destination, Fleet Advisor uses the inventory metadata and metrics from
-- data collector. You can use recommendations before the start of
-- migration to save costs and reduce risks.
--
-- With recommendations, you can explore different target options and
-- compare metrics, so you can make an informed decision when you choose
-- the migration target.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The date when Fleet Advisor created the target engine recommendation.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The recommendation of a target engine for the specified source database.
    data' :: Prelude.Maybe RecommendationData,
    -- | The identifier of the source database for which Fleet Advisor provided
    -- this recommendation.
    databaseId :: Prelude.Maybe Prelude.Text,
    -- | The name of the target engine. Valid values include
    -- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
    -- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | Indicates that this target is the rightsized migration destination.
    preferred :: Prelude.Maybe Prelude.Bool,
    -- | The settings in JSON format for the preferred target engine parameters.
    -- These parameters include capacity, resource utilization, and the usage
    -- type (production, development, or testing).
    settings :: Prelude.Maybe RecommendationSettings,
    -- | The status of the target engine recommendation. Valid values include
    -- @\"alternate\"@, @\"in-progress\"@, @\"not-viable\"@, and
    -- @\"recommended\"@.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'recommendation_createdDate' - The date when Fleet Advisor created the target engine recommendation.
--
-- 'data'', 'recommendation_data' - The recommendation of a target engine for the specified source database.
--
-- 'databaseId', 'recommendation_databaseId' - The identifier of the source database for which Fleet Advisor provided
-- this recommendation.
--
-- 'engineName', 'recommendation_engineName' - The name of the target engine. Valid values include
-- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
-- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
--
-- 'preferred', 'recommendation_preferred' - Indicates that this target is the rightsized migration destination.
--
-- 'settings', 'recommendation_settings' - The settings in JSON format for the preferred target engine parameters.
-- These parameters include capacity, resource utilization, and the usage
-- type (production, development, or testing).
--
-- 'status', 'recommendation_status' - The status of the target engine recommendation. Valid values include
-- @\"alternate\"@, @\"in-progress\"@, @\"not-viable\"@, and
-- @\"recommended\"@.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { createdDate = Prelude.Nothing,
      data' = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      engineName = Prelude.Nothing,
      preferred = Prelude.Nothing,
      settings = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date when Fleet Advisor created the target engine recommendation.
recommendation_createdDate :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_createdDate = Lens.lens (\Recommendation' {createdDate} -> createdDate) (\s@Recommendation' {} a -> s {createdDate = a} :: Recommendation)

-- | The recommendation of a target engine for the specified source database.
recommendation_data :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationData)
recommendation_data = Lens.lens (\Recommendation' {data'} -> data') (\s@Recommendation' {} a -> s {data' = a} :: Recommendation)

-- | The identifier of the source database for which Fleet Advisor provided
-- this recommendation.
recommendation_databaseId :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_databaseId = Lens.lens (\Recommendation' {databaseId} -> databaseId) (\s@Recommendation' {} a -> s {databaseId = a} :: Recommendation)

-- | The name of the target engine. Valid values include
-- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
-- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
recommendation_engineName :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_engineName = Lens.lens (\Recommendation' {engineName} -> engineName) (\s@Recommendation' {} a -> s {engineName = a} :: Recommendation)

-- | Indicates that this target is the rightsized migration destination.
recommendation_preferred :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Bool)
recommendation_preferred = Lens.lens (\Recommendation' {preferred} -> preferred) (\s@Recommendation' {} a -> s {preferred = a} :: Recommendation)

-- | The settings in JSON format for the preferred target engine parameters.
-- These parameters include capacity, resource utilization, and the usage
-- type (production, development, or testing).
recommendation_settings :: Lens.Lens' Recommendation (Prelude.Maybe RecommendationSettings)
recommendation_settings = Lens.lens (\Recommendation' {settings} -> settings) (\s@Recommendation' {} a -> s {settings = a} :: Recommendation)

-- | The status of the target engine recommendation. Valid values include
-- @\"alternate\"@, @\"in-progress\"@, @\"not-viable\"@, and
-- @\"recommended\"@.
recommendation_status :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_status = Lens.lens (\Recommendation' {status} -> status) (\s@Recommendation' {} a -> s {status = a} :: Recommendation)

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Data")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "EngineName")
            Prelude.<*> (x Data..:? "Preferred")
            Prelude.<*> (x Data..:? "Settings")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` preferred
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` status

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf preferred
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf status

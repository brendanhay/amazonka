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
-- Module      : Amazonka.DMS.Types.StartRecommendationsRequestEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.StartRecommendationsRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RecommendationSettings
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the source database to analyze and provide
-- target recommendations according to the specified requirements.
--
-- /See:/ 'newStartRecommendationsRequestEntry' smart constructor.
data StartRecommendationsRequestEntry = StartRecommendationsRequestEntry'
  { -- | The identifier of the source database.
    databaseId :: Prelude.Text,
    -- | The required target engine settings.
    settings :: RecommendationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecommendationsRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseId', 'startRecommendationsRequestEntry_databaseId' - The identifier of the source database.
--
-- 'settings', 'startRecommendationsRequestEntry_settings' - The required target engine settings.
newStartRecommendationsRequestEntry ::
  -- | 'databaseId'
  Prelude.Text ->
  -- | 'settings'
  RecommendationSettings ->
  StartRecommendationsRequestEntry
newStartRecommendationsRequestEntry
  pDatabaseId_
  pSettings_ =
    StartRecommendationsRequestEntry'
      { databaseId =
          pDatabaseId_,
        settings = pSettings_
      }

-- | The identifier of the source database.
startRecommendationsRequestEntry_databaseId :: Lens.Lens' StartRecommendationsRequestEntry Prelude.Text
startRecommendationsRequestEntry_databaseId = Lens.lens (\StartRecommendationsRequestEntry' {databaseId} -> databaseId) (\s@StartRecommendationsRequestEntry' {} a -> s {databaseId = a} :: StartRecommendationsRequestEntry)

-- | The required target engine settings.
startRecommendationsRequestEntry_settings :: Lens.Lens' StartRecommendationsRequestEntry RecommendationSettings
startRecommendationsRequestEntry_settings = Lens.lens (\StartRecommendationsRequestEntry' {settings} -> settings) (\s@StartRecommendationsRequestEntry' {} a -> s {settings = a} :: StartRecommendationsRequestEntry)

instance
  Prelude.Hashable
    StartRecommendationsRequestEntry
  where
  hashWithSalt
    _salt
    StartRecommendationsRequestEntry' {..} =
      _salt
        `Prelude.hashWithSalt` databaseId
        `Prelude.hashWithSalt` settings

instance
  Prelude.NFData
    StartRecommendationsRequestEntry
  where
  rnf StartRecommendationsRequestEntry' {..} =
    Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf settings

instance Data.ToJSON StartRecommendationsRequestEntry where
  toJSON StartRecommendationsRequestEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseId" Data..= databaseId),
            Prelude.Just ("Settings" Data..= settings)
          ]
      )

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
-- Module      : Amazonka.LookoutMetrics.Types.AutoDetectionS3SourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AutoDetectionS3SourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An auto detection source config.
--
-- /See:/ 'newAutoDetectionS3SourceConfig' smart constructor.
data AutoDetectionS3SourceConfig = AutoDetectionS3SourceConfig'
  { -- | The config\'s templated path list.
    templatedPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The config\'s historical data path list.
    historicalDataPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoDetectionS3SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templatedPathList', 'autoDetectionS3SourceConfig_templatedPathList' - The config\'s templated path list.
--
-- 'historicalDataPathList', 'autoDetectionS3SourceConfig_historicalDataPathList' - The config\'s historical data path list.
newAutoDetectionS3SourceConfig ::
  AutoDetectionS3SourceConfig
newAutoDetectionS3SourceConfig =
  AutoDetectionS3SourceConfig'
    { templatedPathList =
        Prelude.Nothing,
      historicalDataPathList = Prelude.Nothing
    }

-- | The config\'s templated path list.
autoDetectionS3SourceConfig_templatedPathList :: Lens.Lens' AutoDetectionS3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
autoDetectionS3SourceConfig_templatedPathList = Lens.lens (\AutoDetectionS3SourceConfig' {templatedPathList} -> templatedPathList) (\s@AutoDetectionS3SourceConfig' {} a -> s {templatedPathList = a} :: AutoDetectionS3SourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | The config\'s historical data path list.
autoDetectionS3SourceConfig_historicalDataPathList :: Lens.Lens' AutoDetectionS3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
autoDetectionS3SourceConfig_historicalDataPathList = Lens.lens (\AutoDetectionS3SourceConfig' {historicalDataPathList} -> historicalDataPathList) (\s@AutoDetectionS3SourceConfig' {} a -> s {historicalDataPathList = a} :: AutoDetectionS3SourceConfig) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AutoDetectionS3SourceConfig where
  hashWithSalt _salt AutoDetectionS3SourceConfig' {..} =
    _salt `Prelude.hashWithSalt` templatedPathList
      `Prelude.hashWithSalt` historicalDataPathList

instance Prelude.NFData AutoDetectionS3SourceConfig where
  rnf AutoDetectionS3SourceConfig' {..} =
    Prelude.rnf templatedPathList
      `Prelude.seq` Prelude.rnf historicalDataPathList

instance Core.ToJSON AutoDetectionS3SourceConfig where
  toJSON AutoDetectionS3SourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplatedPathList" Core..=)
              Prelude.<$> templatedPathList,
            ("HistoricalDataPathList" Core..=)
              Prelude.<$> historicalDataPathList
          ]
      )

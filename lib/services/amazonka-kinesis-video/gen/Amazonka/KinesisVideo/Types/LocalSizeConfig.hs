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
-- Module      : Amazonka.KinesisVideo.Types.LocalSizeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.LocalSizeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.StrategyOnFullSize
import qualified Amazonka.Prelude as Prelude

-- | The configuration details that include the maximum size of the media
-- (@MaxLocalMediaSizeInMB@) that you want to store for a stream on the
-- Edge Agent, as well as the strategy that should be used
-- (@StrategyOnFullSize@) when a stream\'s maximum size has been reached.
--
-- /See:/ 'newLocalSizeConfig' smart constructor.
data LocalSizeConfig = LocalSizeConfig'
  { -- | The overall maximum size of the media that you want to store for a
    -- stream on the Edge Agent.
    maxLocalMediaSizeInMB :: Prelude.Maybe Prelude.Natural,
    -- | The strategy to perform when a stream’s @MaxLocalMediaSizeInMB@ limit is
    -- reached.
    strategyOnFullSize :: Prelude.Maybe StrategyOnFullSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalSizeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLocalMediaSizeInMB', 'localSizeConfig_maxLocalMediaSizeInMB' - The overall maximum size of the media that you want to store for a
-- stream on the Edge Agent.
--
-- 'strategyOnFullSize', 'localSizeConfig_strategyOnFullSize' - The strategy to perform when a stream’s @MaxLocalMediaSizeInMB@ limit is
-- reached.
newLocalSizeConfig ::
  LocalSizeConfig
newLocalSizeConfig =
  LocalSizeConfig'
    { maxLocalMediaSizeInMB =
        Prelude.Nothing,
      strategyOnFullSize = Prelude.Nothing
    }

-- | The overall maximum size of the media that you want to store for a
-- stream on the Edge Agent.
localSizeConfig_maxLocalMediaSizeInMB :: Lens.Lens' LocalSizeConfig (Prelude.Maybe Prelude.Natural)
localSizeConfig_maxLocalMediaSizeInMB = Lens.lens (\LocalSizeConfig' {maxLocalMediaSizeInMB} -> maxLocalMediaSizeInMB) (\s@LocalSizeConfig' {} a -> s {maxLocalMediaSizeInMB = a} :: LocalSizeConfig)

-- | The strategy to perform when a stream’s @MaxLocalMediaSizeInMB@ limit is
-- reached.
localSizeConfig_strategyOnFullSize :: Lens.Lens' LocalSizeConfig (Prelude.Maybe StrategyOnFullSize)
localSizeConfig_strategyOnFullSize = Lens.lens (\LocalSizeConfig' {strategyOnFullSize} -> strategyOnFullSize) (\s@LocalSizeConfig' {} a -> s {strategyOnFullSize = a} :: LocalSizeConfig)

instance Data.FromJSON LocalSizeConfig where
  parseJSON =
    Data.withObject
      "LocalSizeConfig"
      ( \x ->
          LocalSizeConfig'
            Prelude.<$> (x Data..:? "MaxLocalMediaSizeInMB")
            Prelude.<*> (x Data..:? "StrategyOnFullSize")
      )

instance Prelude.Hashable LocalSizeConfig where
  hashWithSalt _salt LocalSizeConfig' {..} =
    _salt `Prelude.hashWithSalt` maxLocalMediaSizeInMB
      `Prelude.hashWithSalt` strategyOnFullSize

instance Prelude.NFData LocalSizeConfig where
  rnf LocalSizeConfig' {..} =
    Prelude.rnf maxLocalMediaSizeInMB
      `Prelude.seq` Prelude.rnf strategyOnFullSize

instance Data.ToJSON LocalSizeConfig where
  toJSON LocalSizeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxLocalMediaSizeInMB" Data..=)
              Prelude.<$> maxLocalMediaSizeInMB,
            ("StrategyOnFullSize" Data..=)
              Prelude.<$> strategyOnFullSize
          ]
      )

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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerOutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerOutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Output data configuration.
--
-- /See:/ 'newEntityRecognizerOutputDataConfig' smart constructor.
data EntityRecognizerOutputDataConfig = EntityRecognizerOutputDataConfig'
  { -- | The Amazon S3 prefix for the data lake location of the flywheel
    -- statistics.
    flywheelStatsS3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelStatsS3Prefix', 'entityRecognizerOutputDataConfig_flywheelStatsS3Prefix' - The Amazon S3 prefix for the data lake location of the flywheel
-- statistics.
newEntityRecognizerOutputDataConfig ::
  EntityRecognizerOutputDataConfig
newEntityRecognizerOutputDataConfig =
  EntityRecognizerOutputDataConfig'
    { flywheelStatsS3Prefix =
        Prelude.Nothing
    }

-- | The Amazon S3 prefix for the data lake location of the flywheel
-- statistics.
entityRecognizerOutputDataConfig_flywheelStatsS3Prefix :: Lens.Lens' EntityRecognizerOutputDataConfig (Prelude.Maybe Prelude.Text)
entityRecognizerOutputDataConfig_flywheelStatsS3Prefix = Lens.lens (\EntityRecognizerOutputDataConfig' {flywheelStatsS3Prefix} -> flywheelStatsS3Prefix) (\s@EntityRecognizerOutputDataConfig' {} a -> s {flywheelStatsS3Prefix = a} :: EntityRecognizerOutputDataConfig)

instance
  Data.FromJSON
    EntityRecognizerOutputDataConfig
  where
  parseJSON =
    Data.withObject
      "EntityRecognizerOutputDataConfig"
      ( \x ->
          EntityRecognizerOutputDataConfig'
            Prelude.<$> (x Data..:? "FlywheelStatsS3Prefix")
      )

instance
  Prelude.Hashable
    EntityRecognizerOutputDataConfig
  where
  hashWithSalt
    _salt
    EntityRecognizerOutputDataConfig' {..} =
      _salt `Prelude.hashWithSalt` flywheelStatsS3Prefix

instance
  Prelude.NFData
    EntityRecognizerOutputDataConfig
  where
  rnf EntityRecognizerOutputDataConfig' {..} =
    Prelude.rnf flywheelStatsS3Prefix

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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatistics

-- |
--
-- /See:/ 'newZonalStatisticsConfigInput' smart constructor.
data ZonalStatisticsConfigInput = ZonalStatisticsConfigInput'
  { targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    statistics :: Prelude.NonEmpty ZonalStatistics,
    zoneS3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZonalStatisticsConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetBands', 'zonalStatisticsConfigInput_targetBands' -
--
-- 'statistics', 'zonalStatisticsConfigInput_statistics' -
--
-- 'zoneS3Path', 'zonalStatisticsConfigInput_zoneS3Path' -
newZonalStatisticsConfigInput ::
  -- | 'statistics'
  Prelude.NonEmpty ZonalStatistics ->
  -- | 'zoneS3Path'
  Prelude.Text ->
  ZonalStatisticsConfigInput
newZonalStatisticsConfigInput
  pStatistics_
  pZoneS3Path_ =
    ZonalStatisticsConfigInput'
      { targetBands =
          Prelude.Nothing,
        statistics = Lens.coerced Lens.# pStatistics_,
        zoneS3Path = pZoneS3Path_
      }

-- |
zonalStatisticsConfigInput_targetBands :: Lens.Lens' ZonalStatisticsConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
zonalStatisticsConfigInput_targetBands = Lens.lens (\ZonalStatisticsConfigInput' {targetBands} -> targetBands) (\s@ZonalStatisticsConfigInput' {} a -> s {targetBands = a} :: ZonalStatisticsConfigInput) Prelude.. Lens.mapping Lens.coerced

-- |
zonalStatisticsConfigInput_statistics :: Lens.Lens' ZonalStatisticsConfigInput (Prelude.NonEmpty ZonalStatistics)
zonalStatisticsConfigInput_statistics = Lens.lens (\ZonalStatisticsConfigInput' {statistics} -> statistics) (\s@ZonalStatisticsConfigInput' {} a -> s {statistics = a} :: ZonalStatisticsConfigInput) Prelude.. Lens.coerced

-- |
zonalStatisticsConfigInput_zoneS3Path :: Lens.Lens' ZonalStatisticsConfigInput Prelude.Text
zonalStatisticsConfigInput_zoneS3Path = Lens.lens (\ZonalStatisticsConfigInput' {zoneS3Path} -> zoneS3Path) (\s@ZonalStatisticsConfigInput' {} a -> s {zoneS3Path = a} :: ZonalStatisticsConfigInput)

instance Data.FromJSON ZonalStatisticsConfigInput where
  parseJSON =
    Data.withObject
      "ZonalStatisticsConfigInput"
      ( \x ->
          ZonalStatisticsConfigInput'
            Prelude.<$> (x Data..:? "TargetBands")
            Prelude.<*> (x Data..: "Statistics")
            Prelude.<*> (x Data..: "ZoneS3Path")
      )

instance Prelude.Hashable ZonalStatisticsConfigInput where
  hashWithSalt _salt ZonalStatisticsConfigInput' {..} =
    _salt `Prelude.hashWithSalt` targetBands
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` zoneS3Path

instance Prelude.NFData ZonalStatisticsConfigInput where
  rnf ZonalStatisticsConfigInput' {..} =
    Prelude.rnf targetBands
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf zoneS3Path

instance Data.ToJSON ZonalStatisticsConfigInput where
  toJSON ZonalStatisticsConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetBands" Data..=) Prelude.<$> targetBands,
            Prelude.Just ("Statistics" Data..= statistics),
            Prelude.Just ("ZoneS3Path" Data..= zoneS3Path)
          ]
      )

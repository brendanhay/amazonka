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
-- Module      : Amazonka.SESV2.Types.DailyVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DailyVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DomainIspPlacement
import Amazonka.SESV2.Types.VolumeStatistics

-- | An object that contains information about the volume of email sent on
-- each day of the analysis period.
--
-- /See:/ 'newDailyVolume' smart constructor.
data DailyVolume = DailyVolume'
  { -- | An object that contains inbox placement metrics for a specific day in
    -- the analysis period.
    volumeStatistics :: Prelude.Maybe VolumeStatistics,
    -- | The date that the DailyVolume metrics apply to, in Unix time.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | An object that contains inbox placement metrics for a specified day in
    -- the analysis period, broken out by the recipient\'s email provider.
    domainIspPlacements :: Prelude.Maybe [DomainIspPlacement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DailyVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeStatistics', 'dailyVolume_volumeStatistics' - An object that contains inbox placement metrics for a specific day in
-- the analysis period.
--
-- 'startDate', 'dailyVolume_startDate' - The date that the DailyVolume metrics apply to, in Unix time.
--
-- 'domainIspPlacements', 'dailyVolume_domainIspPlacements' - An object that contains inbox placement metrics for a specified day in
-- the analysis period, broken out by the recipient\'s email provider.
newDailyVolume ::
  DailyVolume
newDailyVolume =
  DailyVolume'
    { volumeStatistics = Prelude.Nothing,
      startDate = Prelude.Nothing,
      domainIspPlacements = Prelude.Nothing
    }

-- | An object that contains inbox placement metrics for a specific day in
-- the analysis period.
dailyVolume_volumeStatistics :: Lens.Lens' DailyVolume (Prelude.Maybe VolumeStatistics)
dailyVolume_volumeStatistics = Lens.lens (\DailyVolume' {volumeStatistics} -> volumeStatistics) (\s@DailyVolume' {} a -> s {volumeStatistics = a} :: DailyVolume)

-- | The date that the DailyVolume metrics apply to, in Unix time.
dailyVolume_startDate :: Lens.Lens' DailyVolume (Prelude.Maybe Prelude.UTCTime)
dailyVolume_startDate = Lens.lens (\DailyVolume' {startDate} -> startDate) (\s@DailyVolume' {} a -> s {startDate = a} :: DailyVolume) Prelude.. Lens.mapping Data._Time

-- | An object that contains inbox placement metrics for a specified day in
-- the analysis period, broken out by the recipient\'s email provider.
dailyVolume_domainIspPlacements :: Lens.Lens' DailyVolume (Prelude.Maybe [DomainIspPlacement])
dailyVolume_domainIspPlacements = Lens.lens (\DailyVolume' {domainIspPlacements} -> domainIspPlacements) (\s@DailyVolume' {} a -> s {domainIspPlacements = a} :: DailyVolume) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DailyVolume where
  parseJSON =
    Data.withObject
      "DailyVolume"
      ( \x ->
          DailyVolume'
            Prelude.<$> (x Data..:? "VolumeStatistics")
            Prelude.<*> (x Data..:? "StartDate")
            Prelude.<*> ( x Data..:? "DomainIspPlacements"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DailyVolume where
  hashWithSalt _salt DailyVolume' {..} =
    _salt `Prelude.hashWithSalt` volumeStatistics
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` domainIspPlacements

instance Prelude.NFData DailyVolume where
  rnf DailyVolume' {..} =
    Prelude.rnf volumeStatistics
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf domainIspPlacements

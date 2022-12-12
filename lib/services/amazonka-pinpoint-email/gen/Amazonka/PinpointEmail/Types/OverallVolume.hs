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
-- Module      : Amazonka.PinpointEmail.Types.OverallVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.OverallVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types.DomainIspPlacement
import Amazonka.PinpointEmail.Types.VolumeStatistics
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about email that was sent from the
-- selected domain.
--
-- /See:/ 'newOverallVolume' smart constructor.
data OverallVolume = OverallVolume'
  { -- | An object that contains inbox and junk mail placement metrics for
    -- individual email providers.
    domainIspPlacements :: Prelude.Maybe [DomainIspPlacement],
    -- | The percentage of emails that were sent from the domain that were read
    -- by their recipients.
    readRatePercent :: Prelude.Maybe Prelude.Double,
    -- | An object that contains information about the numbers of messages that
    -- arrived in recipients\' inboxes and junk mail folders.
    volumeStatistics :: Prelude.Maybe VolumeStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverallVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainIspPlacements', 'overallVolume_domainIspPlacements' - An object that contains inbox and junk mail placement metrics for
-- individual email providers.
--
-- 'readRatePercent', 'overallVolume_readRatePercent' - The percentage of emails that were sent from the domain that were read
-- by their recipients.
--
-- 'volumeStatistics', 'overallVolume_volumeStatistics' - An object that contains information about the numbers of messages that
-- arrived in recipients\' inboxes and junk mail folders.
newOverallVolume ::
  OverallVolume
newOverallVolume =
  OverallVolume'
    { domainIspPlacements =
        Prelude.Nothing,
      readRatePercent = Prelude.Nothing,
      volumeStatistics = Prelude.Nothing
    }

-- | An object that contains inbox and junk mail placement metrics for
-- individual email providers.
overallVolume_domainIspPlacements :: Lens.Lens' OverallVolume (Prelude.Maybe [DomainIspPlacement])
overallVolume_domainIspPlacements = Lens.lens (\OverallVolume' {domainIspPlacements} -> domainIspPlacements) (\s@OverallVolume' {} a -> s {domainIspPlacements = a} :: OverallVolume) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of emails that were sent from the domain that were read
-- by their recipients.
overallVolume_readRatePercent :: Lens.Lens' OverallVolume (Prelude.Maybe Prelude.Double)
overallVolume_readRatePercent = Lens.lens (\OverallVolume' {readRatePercent} -> readRatePercent) (\s@OverallVolume' {} a -> s {readRatePercent = a} :: OverallVolume)

-- | An object that contains information about the numbers of messages that
-- arrived in recipients\' inboxes and junk mail folders.
overallVolume_volumeStatistics :: Lens.Lens' OverallVolume (Prelude.Maybe VolumeStatistics)
overallVolume_volumeStatistics = Lens.lens (\OverallVolume' {volumeStatistics} -> volumeStatistics) (\s@OverallVolume' {} a -> s {volumeStatistics = a} :: OverallVolume)

instance Data.FromJSON OverallVolume where
  parseJSON =
    Data.withObject
      "OverallVolume"
      ( \x ->
          OverallVolume'
            Prelude.<$> ( x Data..:? "DomainIspPlacements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReadRatePercent")
            Prelude.<*> (x Data..:? "VolumeStatistics")
      )

instance Prelude.Hashable OverallVolume where
  hashWithSalt _salt OverallVolume' {..} =
    _salt `Prelude.hashWithSalt` domainIspPlacements
      `Prelude.hashWithSalt` readRatePercent
      `Prelude.hashWithSalt` volumeStatistics

instance Prelude.NFData OverallVolume where
  rnf OverallVolume' {..} =
    Prelude.rnf domainIspPlacements
      `Prelude.seq` Prelude.rnf readRatePercent
      `Prelude.seq` Prelude.rnf volumeStatistics

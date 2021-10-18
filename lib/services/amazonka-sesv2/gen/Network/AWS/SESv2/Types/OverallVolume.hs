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
-- Module      : Network.AWS.SESv2.Types.OverallVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.OverallVolume where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.DomainIspPlacement
import Network.AWS.SESv2.Types.VolumeStatistics

-- | An object that contains information about email that was sent from the
-- selected domain.
--
-- /See:/ 'newOverallVolume' smart constructor.
data OverallVolume = OverallVolume'
  { -- | The percentage of emails that were sent from the domain that were read
    -- by their recipients.
    readRatePercent :: Prelude.Maybe Prelude.Double,
    -- | An object that contains information about the numbers of messages that
    -- arrived in recipients\' inboxes and junk mail folders.
    volumeStatistics :: Prelude.Maybe VolumeStatistics,
    -- | An object that contains inbox and junk mail placement metrics for
    -- individual email providers.
    domainIspPlacements :: Prelude.Maybe [DomainIspPlacement]
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
-- 'readRatePercent', 'overallVolume_readRatePercent' - The percentage of emails that were sent from the domain that were read
-- by their recipients.
--
-- 'volumeStatistics', 'overallVolume_volumeStatistics' - An object that contains information about the numbers of messages that
-- arrived in recipients\' inboxes and junk mail folders.
--
-- 'domainIspPlacements', 'overallVolume_domainIspPlacements' - An object that contains inbox and junk mail placement metrics for
-- individual email providers.
newOverallVolume ::
  OverallVolume
newOverallVolume =
  OverallVolume'
    { readRatePercent = Prelude.Nothing,
      volumeStatistics = Prelude.Nothing,
      domainIspPlacements = Prelude.Nothing
    }

-- | The percentage of emails that were sent from the domain that were read
-- by their recipients.
overallVolume_readRatePercent :: Lens.Lens' OverallVolume (Prelude.Maybe Prelude.Double)
overallVolume_readRatePercent = Lens.lens (\OverallVolume' {readRatePercent} -> readRatePercent) (\s@OverallVolume' {} a -> s {readRatePercent = a} :: OverallVolume)

-- | An object that contains information about the numbers of messages that
-- arrived in recipients\' inboxes and junk mail folders.
overallVolume_volumeStatistics :: Lens.Lens' OverallVolume (Prelude.Maybe VolumeStatistics)
overallVolume_volumeStatistics = Lens.lens (\OverallVolume' {volumeStatistics} -> volumeStatistics) (\s@OverallVolume' {} a -> s {volumeStatistics = a} :: OverallVolume)

-- | An object that contains inbox and junk mail placement metrics for
-- individual email providers.
overallVolume_domainIspPlacements :: Lens.Lens' OverallVolume (Prelude.Maybe [DomainIspPlacement])
overallVolume_domainIspPlacements = Lens.lens (\OverallVolume' {domainIspPlacements} -> domainIspPlacements) (\s@OverallVolume' {} a -> s {domainIspPlacements = a} :: OverallVolume) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON OverallVolume where
  parseJSON =
    Core.withObject
      "OverallVolume"
      ( \x ->
          OverallVolume'
            Prelude.<$> (x Core..:? "ReadRatePercent")
            Prelude.<*> (x Core..:? "VolumeStatistics")
            Prelude.<*> ( x Core..:? "DomainIspPlacements"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OverallVolume

instance Prelude.NFData OverallVolume

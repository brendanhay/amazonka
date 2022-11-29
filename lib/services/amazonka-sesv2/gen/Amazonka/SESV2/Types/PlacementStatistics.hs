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
-- Module      : Amazonka.SESV2.Types.PlacementStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.PlacementStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains inbox placement data for an email provider.
--
-- /See:/ 'newPlacementStatistics' smart constructor.
data PlacementStatistics = PlacementStatistics'
  { -- | The percentage of emails that didn\'t arrive in recipients\' inboxes at
    -- all during the predictive inbox placement test.
    missingPercentage :: Prelude.Maybe Prelude.Double,
    -- | The percentage of emails that were authenticated by using DomainKeys
    -- Identified Mail (DKIM) during the predictive inbox placement test.
    dkimPercentage :: Prelude.Maybe Prelude.Double,
    -- | The percentage of emails that were authenticated by using Sender Policy
    -- Framework (SPF) during the predictive inbox placement test.
    spfPercentage :: Prelude.Maybe Prelude.Double,
    -- | The percentage of emails that arrived in recipients\' spam or junk mail
    -- folders during the predictive inbox placement test.
    spamPercentage :: Prelude.Maybe Prelude.Double,
    -- | The percentage of emails that arrived in recipients\' inboxes during the
    -- predictive inbox placement test.
    inboxPercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missingPercentage', 'placementStatistics_missingPercentage' - The percentage of emails that didn\'t arrive in recipients\' inboxes at
-- all during the predictive inbox placement test.
--
-- 'dkimPercentage', 'placementStatistics_dkimPercentage' - The percentage of emails that were authenticated by using DomainKeys
-- Identified Mail (DKIM) during the predictive inbox placement test.
--
-- 'spfPercentage', 'placementStatistics_spfPercentage' - The percentage of emails that were authenticated by using Sender Policy
-- Framework (SPF) during the predictive inbox placement test.
--
-- 'spamPercentage', 'placementStatistics_spamPercentage' - The percentage of emails that arrived in recipients\' spam or junk mail
-- folders during the predictive inbox placement test.
--
-- 'inboxPercentage', 'placementStatistics_inboxPercentage' - The percentage of emails that arrived in recipients\' inboxes during the
-- predictive inbox placement test.
newPlacementStatistics ::
  PlacementStatistics
newPlacementStatistics =
  PlacementStatistics'
    { missingPercentage =
        Prelude.Nothing,
      dkimPercentage = Prelude.Nothing,
      spfPercentage = Prelude.Nothing,
      spamPercentage = Prelude.Nothing,
      inboxPercentage = Prelude.Nothing
    }

-- | The percentage of emails that didn\'t arrive in recipients\' inboxes at
-- all during the predictive inbox placement test.
placementStatistics_missingPercentage :: Lens.Lens' PlacementStatistics (Prelude.Maybe Prelude.Double)
placementStatistics_missingPercentage = Lens.lens (\PlacementStatistics' {missingPercentage} -> missingPercentage) (\s@PlacementStatistics' {} a -> s {missingPercentage = a} :: PlacementStatistics)

-- | The percentage of emails that were authenticated by using DomainKeys
-- Identified Mail (DKIM) during the predictive inbox placement test.
placementStatistics_dkimPercentage :: Lens.Lens' PlacementStatistics (Prelude.Maybe Prelude.Double)
placementStatistics_dkimPercentage = Lens.lens (\PlacementStatistics' {dkimPercentage} -> dkimPercentage) (\s@PlacementStatistics' {} a -> s {dkimPercentage = a} :: PlacementStatistics)

-- | The percentage of emails that were authenticated by using Sender Policy
-- Framework (SPF) during the predictive inbox placement test.
placementStatistics_spfPercentage :: Lens.Lens' PlacementStatistics (Prelude.Maybe Prelude.Double)
placementStatistics_spfPercentage = Lens.lens (\PlacementStatistics' {spfPercentage} -> spfPercentage) (\s@PlacementStatistics' {} a -> s {spfPercentage = a} :: PlacementStatistics)

-- | The percentage of emails that arrived in recipients\' spam or junk mail
-- folders during the predictive inbox placement test.
placementStatistics_spamPercentage :: Lens.Lens' PlacementStatistics (Prelude.Maybe Prelude.Double)
placementStatistics_spamPercentage = Lens.lens (\PlacementStatistics' {spamPercentage} -> spamPercentage) (\s@PlacementStatistics' {} a -> s {spamPercentage = a} :: PlacementStatistics)

-- | The percentage of emails that arrived in recipients\' inboxes during the
-- predictive inbox placement test.
placementStatistics_inboxPercentage :: Lens.Lens' PlacementStatistics (Prelude.Maybe Prelude.Double)
placementStatistics_inboxPercentage = Lens.lens (\PlacementStatistics' {inboxPercentage} -> inboxPercentage) (\s@PlacementStatistics' {} a -> s {inboxPercentage = a} :: PlacementStatistics)

instance Core.FromJSON PlacementStatistics where
  parseJSON =
    Core.withObject
      "PlacementStatistics"
      ( \x ->
          PlacementStatistics'
            Prelude.<$> (x Core..:? "MissingPercentage")
            Prelude.<*> (x Core..:? "DkimPercentage")
            Prelude.<*> (x Core..:? "SpfPercentage")
            Prelude.<*> (x Core..:? "SpamPercentage")
            Prelude.<*> (x Core..:? "InboxPercentage")
      )

instance Prelude.Hashable PlacementStatistics where
  hashWithSalt _salt PlacementStatistics' {..} =
    _salt `Prelude.hashWithSalt` missingPercentage
      `Prelude.hashWithSalt` dkimPercentage
      `Prelude.hashWithSalt` spfPercentage
      `Prelude.hashWithSalt` spamPercentage
      `Prelude.hashWithSalt` inboxPercentage

instance Prelude.NFData PlacementStatistics where
  rnf PlacementStatistics' {..} =
    Prelude.rnf missingPercentage
      `Prelude.seq` Prelude.rnf dkimPercentage
      `Prelude.seq` Prelude.rnf spfPercentage
      `Prelude.seq` Prelude.rnf spamPercentage
      `Prelude.seq` Prelude.rnf inboxPercentage

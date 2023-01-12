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
-- Module      : Amazonka.SESV2.Types.VolumeStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.VolumeStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the amount of email that was
-- delivered to recipients.
--
-- /See:/ 'newVolumeStatistics' smart constructor.
data VolumeStatistics = VolumeStatistics'
  { -- | The total number of emails that arrived in recipients\' inboxes.
    inboxRawCount :: Prelude.Maybe Prelude.Integer,
    -- | An estimate of the percentage of emails sent from the current domain
    -- that will arrive in recipients\' inboxes.
    projectedInbox :: Prelude.Maybe Prelude.Integer,
    -- | An estimate of the percentage of emails sent from the current domain
    -- that will arrive in recipients\' spam or junk mail folders.
    projectedSpam :: Prelude.Maybe Prelude.Integer,
    -- | The total number of emails that arrived in recipients\' spam or junk
    -- mail folders.
    spamRawCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboxRawCount', 'volumeStatistics_inboxRawCount' - The total number of emails that arrived in recipients\' inboxes.
--
-- 'projectedInbox', 'volumeStatistics_projectedInbox' - An estimate of the percentage of emails sent from the current domain
-- that will arrive in recipients\' inboxes.
--
-- 'projectedSpam', 'volumeStatistics_projectedSpam' - An estimate of the percentage of emails sent from the current domain
-- that will arrive in recipients\' spam or junk mail folders.
--
-- 'spamRawCount', 'volumeStatistics_spamRawCount' - The total number of emails that arrived in recipients\' spam or junk
-- mail folders.
newVolumeStatistics ::
  VolumeStatistics
newVolumeStatistics =
  VolumeStatistics'
    { inboxRawCount = Prelude.Nothing,
      projectedInbox = Prelude.Nothing,
      projectedSpam = Prelude.Nothing,
      spamRawCount = Prelude.Nothing
    }

-- | The total number of emails that arrived in recipients\' inboxes.
volumeStatistics_inboxRawCount :: Lens.Lens' VolumeStatistics (Prelude.Maybe Prelude.Integer)
volumeStatistics_inboxRawCount = Lens.lens (\VolumeStatistics' {inboxRawCount} -> inboxRawCount) (\s@VolumeStatistics' {} a -> s {inboxRawCount = a} :: VolumeStatistics)

-- | An estimate of the percentage of emails sent from the current domain
-- that will arrive in recipients\' inboxes.
volumeStatistics_projectedInbox :: Lens.Lens' VolumeStatistics (Prelude.Maybe Prelude.Integer)
volumeStatistics_projectedInbox = Lens.lens (\VolumeStatistics' {projectedInbox} -> projectedInbox) (\s@VolumeStatistics' {} a -> s {projectedInbox = a} :: VolumeStatistics)

-- | An estimate of the percentage of emails sent from the current domain
-- that will arrive in recipients\' spam or junk mail folders.
volumeStatistics_projectedSpam :: Lens.Lens' VolumeStatistics (Prelude.Maybe Prelude.Integer)
volumeStatistics_projectedSpam = Lens.lens (\VolumeStatistics' {projectedSpam} -> projectedSpam) (\s@VolumeStatistics' {} a -> s {projectedSpam = a} :: VolumeStatistics)

-- | The total number of emails that arrived in recipients\' spam or junk
-- mail folders.
volumeStatistics_spamRawCount :: Lens.Lens' VolumeStatistics (Prelude.Maybe Prelude.Integer)
volumeStatistics_spamRawCount = Lens.lens (\VolumeStatistics' {spamRawCount} -> spamRawCount) (\s@VolumeStatistics' {} a -> s {spamRawCount = a} :: VolumeStatistics)

instance Data.FromJSON VolumeStatistics where
  parseJSON =
    Data.withObject
      "VolumeStatistics"
      ( \x ->
          VolumeStatistics'
            Prelude.<$> (x Data..:? "InboxRawCount")
            Prelude.<*> (x Data..:? "ProjectedInbox")
            Prelude.<*> (x Data..:? "ProjectedSpam")
            Prelude.<*> (x Data..:? "SpamRawCount")
      )

instance Prelude.Hashable VolumeStatistics where
  hashWithSalt _salt VolumeStatistics' {..} =
    _salt `Prelude.hashWithSalt` inboxRawCount
      `Prelude.hashWithSalt` projectedInbox
      `Prelude.hashWithSalt` projectedSpam
      `Prelude.hashWithSalt` spamRawCount

instance Prelude.NFData VolumeStatistics where
  rnf VolumeStatistics' {..} =
    Prelude.rnf inboxRawCount
      `Prelude.seq` Prelude.rnf projectedInbox
      `Prelude.seq` Prelude.rnf projectedSpam
      `Prelude.seq` Prelude.rnf spamRawCount

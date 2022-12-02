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
-- Module      : Amazonka.Snowball.Types.LongTermPricingListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.LongTermPricingListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.LongTermPricingType
import Amazonka.Snowball.Types.SnowballType

-- | Each @LongTermPricingListEntry@ object contains information about a
-- long-term pricing type.
--
-- /See:/ 'newLongTermPricingListEntry' smart constructor.
data LongTermPricingListEntry = LongTermPricingListEntry'
  { -- | The status of the long-term pricing type.
    longTermPricingStatus :: Prelude.Maybe Prelude.Text,
    -- | The end date the long-term pricing contract.
    longTermPricingEndDate :: Prelude.Maybe Data.POSIX,
    -- | The current active jobs on the device the long-term pricing type.
    currentActiveJob :: Prelude.Maybe Prelude.Text,
    -- | The type of long-term pricing that was selected for the device.
    longTermPricingType :: Prelude.Maybe LongTermPricingType,
    -- | If set to @true@, specifies that the current long-term pricing type for
    -- the device should be automatically renewed before the long-term pricing
    -- contract expires.
    isLongTermPricingAutoRenew :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the long-term pricing type for the device.
    longTermPricingId :: Prelude.Maybe Prelude.Text,
    -- | A new device that replaces a device that is ordered with long-term
    -- pricing.
    replacementJob :: Prelude.Maybe Prelude.Text,
    -- | The start date of the long-term pricing contract.
    longTermPricingStartDate :: Prelude.Maybe Data.POSIX,
    -- | The type of Snow Family devices associated with this long-term pricing
    -- job.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The IDs of the jobs that are associated with a long-term pricing type.
    jobIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LongTermPricingListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longTermPricingStatus', 'longTermPricingListEntry_longTermPricingStatus' - The status of the long-term pricing type.
--
-- 'longTermPricingEndDate', 'longTermPricingListEntry_longTermPricingEndDate' - The end date the long-term pricing contract.
--
-- 'currentActiveJob', 'longTermPricingListEntry_currentActiveJob' - The current active jobs on the device the long-term pricing type.
--
-- 'longTermPricingType', 'longTermPricingListEntry_longTermPricingType' - The type of long-term pricing that was selected for the device.
--
-- 'isLongTermPricingAutoRenew', 'longTermPricingListEntry_isLongTermPricingAutoRenew' - If set to @true@, specifies that the current long-term pricing type for
-- the device should be automatically renewed before the long-term pricing
-- contract expires.
--
-- 'longTermPricingId', 'longTermPricingListEntry_longTermPricingId' - The ID of the long-term pricing type for the device.
--
-- 'replacementJob', 'longTermPricingListEntry_replacementJob' - A new device that replaces a device that is ordered with long-term
-- pricing.
--
-- 'longTermPricingStartDate', 'longTermPricingListEntry_longTermPricingStartDate' - The start date of the long-term pricing contract.
--
-- 'snowballType', 'longTermPricingListEntry_snowballType' - The type of Snow Family devices associated with this long-term pricing
-- job.
--
-- 'jobIds', 'longTermPricingListEntry_jobIds' - The IDs of the jobs that are associated with a long-term pricing type.
newLongTermPricingListEntry ::
  LongTermPricingListEntry
newLongTermPricingListEntry =
  LongTermPricingListEntry'
    { longTermPricingStatus =
        Prelude.Nothing,
      longTermPricingEndDate = Prelude.Nothing,
      currentActiveJob = Prelude.Nothing,
      longTermPricingType = Prelude.Nothing,
      isLongTermPricingAutoRenew = Prelude.Nothing,
      longTermPricingId = Prelude.Nothing,
      replacementJob = Prelude.Nothing,
      longTermPricingStartDate = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      jobIds = Prelude.Nothing
    }

-- | The status of the long-term pricing type.
longTermPricingListEntry_longTermPricingStatus :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_longTermPricingStatus = Lens.lens (\LongTermPricingListEntry' {longTermPricingStatus} -> longTermPricingStatus) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingStatus = a} :: LongTermPricingListEntry)

-- | The end date the long-term pricing contract.
longTermPricingListEntry_longTermPricingEndDate :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.UTCTime)
longTermPricingListEntry_longTermPricingEndDate = Lens.lens (\LongTermPricingListEntry' {longTermPricingEndDate} -> longTermPricingEndDate) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingEndDate = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Data._Time

-- | The current active jobs on the device the long-term pricing type.
longTermPricingListEntry_currentActiveJob :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_currentActiveJob = Lens.lens (\LongTermPricingListEntry' {currentActiveJob} -> currentActiveJob) (\s@LongTermPricingListEntry' {} a -> s {currentActiveJob = a} :: LongTermPricingListEntry)

-- | The type of long-term pricing that was selected for the device.
longTermPricingListEntry_longTermPricingType :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe LongTermPricingType)
longTermPricingListEntry_longTermPricingType = Lens.lens (\LongTermPricingListEntry' {longTermPricingType} -> longTermPricingType) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingType = a} :: LongTermPricingListEntry)

-- | If set to @true@, specifies that the current long-term pricing type for
-- the device should be automatically renewed before the long-term pricing
-- contract expires.
longTermPricingListEntry_isLongTermPricingAutoRenew :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Bool)
longTermPricingListEntry_isLongTermPricingAutoRenew = Lens.lens (\LongTermPricingListEntry' {isLongTermPricingAutoRenew} -> isLongTermPricingAutoRenew) (\s@LongTermPricingListEntry' {} a -> s {isLongTermPricingAutoRenew = a} :: LongTermPricingListEntry)

-- | The ID of the long-term pricing type for the device.
longTermPricingListEntry_longTermPricingId :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_longTermPricingId = Lens.lens (\LongTermPricingListEntry' {longTermPricingId} -> longTermPricingId) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingId = a} :: LongTermPricingListEntry)

-- | A new device that replaces a device that is ordered with long-term
-- pricing.
longTermPricingListEntry_replacementJob :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_replacementJob = Lens.lens (\LongTermPricingListEntry' {replacementJob} -> replacementJob) (\s@LongTermPricingListEntry' {} a -> s {replacementJob = a} :: LongTermPricingListEntry)

-- | The start date of the long-term pricing contract.
longTermPricingListEntry_longTermPricingStartDate :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.UTCTime)
longTermPricingListEntry_longTermPricingStartDate = Lens.lens (\LongTermPricingListEntry' {longTermPricingStartDate} -> longTermPricingStartDate) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingStartDate = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Data._Time

-- | The type of Snow Family devices associated with this long-term pricing
-- job.
longTermPricingListEntry_snowballType :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe SnowballType)
longTermPricingListEntry_snowballType = Lens.lens (\LongTermPricingListEntry' {snowballType} -> snowballType) (\s@LongTermPricingListEntry' {} a -> s {snowballType = a} :: LongTermPricingListEntry)

-- | The IDs of the jobs that are associated with a long-term pricing type.
longTermPricingListEntry_jobIds :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe [Prelude.Text])
longTermPricingListEntry_jobIds = Lens.lens (\LongTermPricingListEntry' {jobIds} -> jobIds) (\s@LongTermPricingListEntry' {} a -> s {jobIds = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LongTermPricingListEntry where
  parseJSON =
    Data.withObject
      "LongTermPricingListEntry"
      ( \x ->
          LongTermPricingListEntry'
            Prelude.<$> (x Data..:? "LongTermPricingStatus")
            Prelude.<*> (x Data..:? "LongTermPricingEndDate")
            Prelude.<*> (x Data..:? "CurrentActiveJob")
            Prelude.<*> (x Data..:? "LongTermPricingType")
            Prelude.<*> (x Data..:? "IsLongTermPricingAutoRenew")
            Prelude.<*> (x Data..:? "LongTermPricingId")
            Prelude.<*> (x Data..:? "ReplacementJob")
            Prelude.<*> (x Data..:? "LongTermPricingStartDate")
            Prelude.<*> (x Data..:? "SnowballType")
            Prelude.<*> (x Data..:? "JobIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LongTermPricingListEntry where
  hashWithSalt _salt LongTermPricingListEntry' {..} =
    _salt `Prelude.hashWithSalt` longTermPricingStatus
      `Prelude.hashWithSalt` longTermPricingEndDate
      `Prelude.hashWithSalt` currentActiveJob
      `Prelude.hashWithSalt` longTermPricingType
      `Prelude.hashWithSalt` isLongTermPricingAutoRenew
      `Prelude.hashWithSalt` longTermPricingId
      `Prelude.hashWithSalt` replacementJob
      `Prelude.hashWithSalt` longTermPricingStartDate
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` jobIds

instance Prelude.NFData LongTermPricingListEntry where
  rnf LongTermPricingListEntry' {..} =
    Prelude.rnf longTermPricingStatus
      `Prelude.seq` Prelude.rnf longTermPricingEndDate
      `Prelude.seq` Prelude.rnf currentActiveJob
      `Prelude.seq` Prelude.rnf longTermPricingType
      `Prelude.seq` Prelude.rnf isLongTermPricingAutoRenew
      `Prelude.seq` Prelude.rnf longTermPricingId
      `Prelude.seq` Prelude.rnf replacementJob
      `Prelude.seq` Prelude.rnf longTermPricingStartDate
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf jobIds

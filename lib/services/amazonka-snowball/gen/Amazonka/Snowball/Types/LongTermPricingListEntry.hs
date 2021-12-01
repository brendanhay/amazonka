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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.LongTermPricingListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.LongTermPricingType
import Amazonka.Snowball.Types.SnowballType

-- | Each @LongTermPricingListEntry@ object contains information about a
-- long-term pricing type.
--
-- /See:/ 'newLongTermPricingListEntry' smart constructor.
data LongTermPricingListEntry = LongTermPricingListEntry'
  { -- | The type of long-term pricing that was selected for the device.
    longTermPricingType :: Prelude.Maybe LongTermPricingType,
    -- | The start date of the long-term pricing contract.
    longTermPricingStartDate :: Prelude.Maybe Core.POSIX,
    -- | The type of AWS Snow Family device associated with this long-term
    -- pricing job.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The ID of the long-term pricing type for the device.
    longTermPricingId :: Prelude.Maybe Prelude.Text,
    -- | The end date the long-term pricing contract.
    longTermPricingEndDate :: Prelude.Maybe Core.POSIX,
    -- | The current active jobs on the device the long-term pricing type.
    currentActiveJob :: Prelude.Maybe Prelude.Text,
    -- | If set to @true@, specifies that the current long-term pricing type for
    -- the device should be automatically renewed before the long-term pricing
    -- contract expires.
    isLongTermPricingAutoRenew :: Prelude.Maybe Prelude.Bool,
    -- | The status of the long-term pricing type.
    longTermPricingStatus :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the jobs that are associated with a long-term pricing type.
    jobIds :: Prelude.Maybe [Prelude.Text],
    -- | A new device that replaces a device that is ordered with long-term
    -- pricing.
    replacementJob :: Prelude.Maybe Prelude.Text
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
-- 'longTermPricingType', 'longTermPricingListEntry_longTermPricingType' - The type of long-term pricing that was selected for the device.
--
-- 'longTermPricingStartDate', 'longTermPricingListEntry_longTermPricingStartDate' - The start date of the long-term pricing contract.
--
-- 'snowballType', 'longTermPricingListEntry_snowballType' - The type of AWS Snow Family device associated with this long-term
-- pricing job.
--
-- 'longTermPricingId', 'longTermPricingListEntry_longTermPricingId' - The ID of the long-term pricing type for the device.
--
-- 'longTermPricingEndDate', 'longTermPricingListEntry_longTermPricingEndDate' - The end date the long-term pricing contract.
--
-- 'currentActiveJob', 'longTermPricingListEntry_currentActiveJob' - The current active jobs on the device the long-term pricing type.
--
-- 'isLongTermPricingAutoRenew', 'longTermPricingListEntry_isLongTermPricingAutoRenew' - If set to @true@, specifies that the current long-term pricing type for
-- the device should be automatically renewed before the long-term pricing
-- contract expires.
--
-- 'longTermPricingStatus', 'longTermPricingListEntry_longTermPricingStatus' - The status of the long-term pricing type.
--
-- 'jobIds', 'longTermPricingListEntry_jobIds' - The IDs of the jobs that are associated with a long-term pricing type.
--
-- 'replacementJob', 'longTermPricingListEntry_replacementJob' - A new device that replaces a device that is ordered with long-term
-- pricing.
newLongTermPricingListEntry ::
  LongTermPricingListEntry
newLongTermPricingListEntry =
  LongTermPricingListEntry'
    { longTermPricingType =
        Prelude.Nothing,
      longTermPricingStartDate = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      longTermPricingId = Prelude.Nothing,
      longTermPricingEndDate = Prelude.Nothing,
      currentActiveJob = Prelude.Nothing,
      isLongTermPricingAutoRenew = Prelude.Nothing,
      longTermPricingStatus = Prelude.Nothing,
      jobIds = Prelude.Nothing,
      replacementJob = Prelude.Nothing
    }

-- | The type of long-term pricing that was selected for the device.
longTermPricingListEntry_longTermPricingType :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe LongTermPricingType)
longTermPricingListEntry_longTermPricingType = Lens.lens (\LongTermPricingListEntry' {longTermPricingType} -> longTermPricingType) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingType = a} :: LongTermPricingListEntry)

-- | The start date of the long-term pricing contract.
longTermPricingListEntry_longTermPricingStartDate :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.UTCTime)
longTermPricingListEntry_longTermPricingStartDate = Lens.lens (\LongTermPricingListEntry' {longTermPricingStartDate} -> longTermPricingStartDate) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingStartDate = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Core._Time

-- | The type of AWS Snow Family device associated with this long-term
-- pricing job.
longTermPricingListEntry_snowballType :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe SnowballType)
longTermPricingListEntry_snowballType = Lens.lens (\LongTermPricingListEntry' {snowballType} -> snowballType) (\s@LongTermPricingListEntry' {} a -> s {snowballType = a} :: LongTermPricingListEntry)

-- | The ID of the long-term pricing type for the device.
longTermPricingListEntry_longTermPricingId :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_longTermPricingId = Lens.lens (\LongTermPricingListEntry' {longTermPricingId} -> longTermPricingId) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingId = a} :: LongTermPricingListEntry)

-- | The end date the long-term pricing contract.
longTermPricingListEntry_longTermPricingEndDate :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.UTCTime)
longTermPricingListEntry_longTermPricingEndDate = Lens.lens (\LongTermPricingListEntry' {longTermPricingEndDate} -> longTermPricingEndDate) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingEndDate = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Core._Time

-- | The current active jobs on the device the long-term pricing type.
longTermPricingListEntry_currentActiveJob :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_currentActiveJob = Lens.lens (\LongTermPricingListEntry' {currentActiveJob} -> currentActiveJob) (\s@LongTermPricingListEntry' {} a -> s {currentActiveJob = a} :: LongTermPricingListEntry)

-- | If set to @true@, specifies that the current long-term pricing type for
-- the device should be automatically renewed before the long-term pricing
-- contract expires.
longTermPricingListEntry_isLongTermPricingAutoRenew :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Bool)
longTermPricingListEntry_isLongTermPricingAutoRenew = Lens.lens (\LongTermPricingListEntry' {isLongTermPricingAutoRenew} -> isLongTermPricingAutoRenew) (\s@LongTermPricingListEntry' {} a -> s {isLongTermPricingAutoRenew = a} :: LongTermPricingListEntry)

-- | The status of the long-term pricing type.
longTermPricingListEntry_longTermPricingStatus :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_longTermPricingStatus = Lens.lens (\LongTermPricingListEntry' {longTermPricingStatus} -> longTermPricingStatus) (\s@LongTermPricingListEntry' {} a -> s {longTermPricingStatus = a} :: LongTermPricingListEntry)

-- | The IDs of the jobs that are associated with a long-term pricing type.
longTermPricingListEntry_jobIds :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe [Prelude.Text])
longTermPricingListEntry_jobIds = Lens.lens (\LongTermPricingListEntry' {jobIds} -> jobIds) (\s@LongTermPricingListEntry' {} a -> s {jobIds = a} :: LongTermPricingListEntry) Prelude.. Lens.mapping Lens.coerced

-- | A new device that replaces a device that is ordered with long-term
-- pricing.
longTermPricingListEntry_replacementJob :: Lens.Lens' LongTermPricingListEntry (Prelude.Maybe Prelude.Text)
longTermPricingListEntry_replacementJob = Lens.lens (\LongTermPricingListEntry' {replacementJob} -> replacementJob) (\s@LongTermPricingListEntry' {} a -> s {replacementJob = a} :: LongTermPricingListEntry)

instance Core.FromJSON LongTermPricingListEntry where
  parseJSON =
    Core.withObject
      "LongTermPricingListEntry"
      ( \x ->
          LongTermPricingListEntry'
            Prelude.<$> (x Core..:? "LongTermPricingType")
            Prelude.<*> (x Core..:? "LongTermPricingStartDate")
            Prelude.<*> (x Core..:? "SnowballType")
            Prelude.<*> (x Core..:? "LongTermPricingId")
            Prelude.<*> (x Core..:? "LongTermPricingEndDate")
            Prelude.<*> (x Core..:? "CurrentActiveJob")
            Prelude.<*> (x Core..:? "IsLongTermPricingAutoRenew")
            Prelude.<*> (x Core..:? "LongTermPricingStatus")
            Prelude.<*> (x Core..:? "JobIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReplacementJob")
      )

instance Prelude.Hashable LongTermPricingListEntry where
  hashWithSalt salt' LongTermPricingListEntry' {..} =
    salt' `Prelude.hashWithSalt` replacementJob
      `Prelude.hashWithSalt` jobIds
      `Prelude.hashWithSalt` longTermPricingStatus
      `Prelude.hashWithSalt` isLongTermPricingAutoRenew
      `Prelude.hashWithSalt` currentActiveJob
      `Prelude.hashWithSalt` longTermPricingEndDate
      `Prelude.hashWithSalt` longTermPricingId
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` longTermPricingStartDate
      `Prelude.hashWithSalt` longTermPricingType

instance Prelude.NFData LongTermPricingListEntry where
  rnf LongTermPricingListEntry' {..} =
    Prelude.rnf longTermPricingType
      `Prelude.seq` Prelude.rnf replacementJob
      `Prelude.seq` Prelude.rnf jobIds
      `Prelude.seq` Prelude.rnf longTermPricingStatus
      `Prelude.seq` Prelude.rnf isLongTermPricingAutoRenew
      `Prelude.seq` Prelude.rnf currentActiveJob
      `Prelude.seq` Prelude.rnf longTermPricingEndDate
      `Prelude.seq` Prelude.rnf longTermPricingId
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf longTermPricingStartDate

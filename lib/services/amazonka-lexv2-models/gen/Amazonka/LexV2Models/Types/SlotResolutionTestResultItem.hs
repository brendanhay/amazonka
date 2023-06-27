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
-- Module      : Amazonka.LexV2Models.Types.SlotResolutionTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotResolutionTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotResolutionTestResultItemCounts
import qualified Amazonka.Prelude as Prelude

-- | Information about the success and failure rate of slot resolution in the
-- results of a test execution.
--
-- /See:/ 'newSlotResolutionTestResultItem' smart constructor.
data SlotResolutionTestResultItem = SlotResolutionTestResultItem'
  { -- | The name of the slot.
    slotName :: Prelude.Text,
    -- | A result for slot resolution in the results of a test execution.
    resultCounts :: SlotResolutionTestResultItemCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotResolutionTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotName', 'slotResolutionTestResultItem_slotName' - The name of the slot.
--
-- 'resultCounts', 'slotResolutionTestResultItem_resultCounts' - A result for slot resolution in the results of a test execution.
newSlotResolutionTestResultItem ::
  -- | 'slotName'
  Prelude.Text ->
  -- | 'resultCounts'
  SlotResolutionTestResultItemCounts ->
  SlotResolutionTestResultItem
newSlotResolutionTestResultItem
  pSlotName_
  pResultCounts_ =
    SlotResolutionTestResultItem'
      { slotName =
          pSlotName_,
        resultCounts = pResultCounts_
      }

-- | The name of the slot.
slotResolutionTestResultItem_slotName :: Lens.Lens' SlotResolutionTestResultItem Prelude.Text
slotResolutionTestResultItem_slotName = Lens.lens (\SlotResolutionTestResultItem' {slotName} -> slotName) (\s@SlotResolutionTestResultItem' {} a -> s {slotName = a} :: SlotResolutionTestResultItem)

-- | A result for slot resolution in the results of a test execution.
slotResolutionTestResultItem_resultCounts :: Lens.Lens' SlotResolutionTestResultItem SlotResolutionTestResultItemCounts
slotResolutionTestResultItem_resultCounts = Lens.lens (\SlotResolutionTestResultItem' {resultCounts} -> resultCounts) (\s@SlotResolutionTestResultItem' {} a -> s {resultCounts = a} :: SlotResolutionTestResultItem)

instance Data.FromJSON SlotResolutionTestResultItem where
  parseJSON =
    Data.withObject
      "SlotResolutionTestResultItem"
      ( \x ->
          SlotResolutionTestResultItem'
            Prelude.<$> (x Data..: "slotName")
            Prelude.<*> (x Data..: "resultCounts")
      )

instance
  Prelude.Hashable
    SlotResolutionTestResultItem
  where
  hashWithSalt _salt SlotResolutionTestResultItem' {..} =
    _salt
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` resultCounts

instance Prelude.NFData SlotResolutionTestResultItem where
  rnf SlotResolutionTestResultItem' {..} =
    Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf resultCounts

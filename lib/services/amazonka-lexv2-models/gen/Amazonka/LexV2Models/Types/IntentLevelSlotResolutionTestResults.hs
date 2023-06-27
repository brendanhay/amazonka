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
-- Module      : Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | Indicates the success or failure of slots at the intent level.
--
-- /See:/ 'newIntentLevelSlotResolutionTestResults' smart constructor.
data IntentLevelSlotResolutionTestResults = IntentLevelSlotResolutionTestResults'
  { -- | Indicates the items for the slot level resolution for the intents.
    items :: [IntentLevelSlotResolutionTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentLevelSlotResolutionTestResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'intentLevelSlotResolutionTestResults_items' - Indicates the items for the slot level resolution for the intents.
newIntentLevelSlotResolutionTestResults ::
  IntentLevelSlotResolutionTestResults
newIntentLevelSlotResolutionTestResults =
  IntentLevelSlotResolutionTestResults'
    { items =
        Prelude.mempty
    }

-- | Indicates the items for the slot level resolution for the intents.
intentLevelSlotResolutionTestResults_items :: Lens.Lens' IntentLevelSlotResolutionTestResults [IntentLevelSlotResolutionTestResultItem]
intentLevelSlotResolutionTestResults_items = Lens.lens (\IntentLevelSlotResolutionTestResults' {items} -> items) (\s@IntentLevelSlotResolutionTestResults' {} a -> s {items = a} :: IntentLevelSlotResolutionTestResults) Prelude.. Lens.coerced

instance
  Data.FromJSON
    IntentLevelSlotResolutionTestResults
  where
  parseJSON =
    Data.withObject
      "IntentLevelSlotResolutionTestResults"
      ( \x ->
          IntentLevelSlotResolutionTestResults'
            Prelude.<$> (x Data..:? "items" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    IntentLevelSlotResolutionTestResults
  where
  hashWithSalt
    _salt
    IntentLevelSlotResolutionTestResults' {..} =
      _salt `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    IntentLevelSlotResolutionTestResults
  where
  rnf IntentLevelSlotResolutionTestResults' {..} =
    Prelude.rnf items

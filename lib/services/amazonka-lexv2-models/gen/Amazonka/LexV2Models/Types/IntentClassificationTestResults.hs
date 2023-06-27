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
-- Module      : Amazonka.LexV2Models.Types.IntentClassificationTestResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentClassificationTestResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentClassificationTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | Information for the results of the intent classification test.
--
-- /See:/ 'newIntentClassificationTestResults' smart constructor.
data IntentClassificationTestResults = IntentClassificationTestResults'
  { -- | A list of the results for the intent classification test.
    items :: [IntentClassificationTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentClassificationTestResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'intentClassificationTestResults_items' - A list of the results for the intent classification test.
newIntentClassificationTestResults ::
  IntentClassificationTestResults
newIntentClassificationTestResults =
  IntentClassificationTestResults'
    { items =
        Prelude.mempty
    }

-- | A list of the results for the intent classification test.
intentClassificationTestResults_items :: Lens.Lens' IntentClassificationTestResults [IntentClassificationTestResultItem]
intentClassificationTestResults_items = Lens.lens (\IntentClassificationTestResults' {items} -> items) (\s@IntentClassificationTestResults' {} a -> s {items = a} :: IntentClassificationTestResults) Prelude.. Lens.coerced

instance
  Data.FromJSON
    IntentClassificationTestResults
  where
  parseJSON =
    Data.withObject
      "IntentClassificationTestResults"
      ( \x ->
          IntentClassificationTestResults'
            Prelude.<$> (x Data..:? "items" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    IntentClassificationTestResults
  where
  hashWithSalt
    _salt
    IntentClassificationTestResults' {..} =
      _salt `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    IntentClassificationTestResults
  where
  rnf IntentClassificationTestResults' {..} =
    Prelude.rnf items

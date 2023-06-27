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
-- Module      : Amazonka.LexV2Models.Types.UtteranceLevelTestResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UtteranceLevelTestResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.UtteranceLevelTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the utterances in the results of the test set
-- execution.
--
-- /See:/ 'newUtteranceLevelTestResults' smart constructor.
data UtteranceLevelTestResults = UtteranceLevelTestResults'
  { -- | Contains information about an utterance in the results of the test set
    -- execution.
    items :: [UtteranceLevelTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceLevelTestResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'utteranceLevelTestResults_items' - Contains information about an utterance in the results of the test set
-- execution.
newUtteranceLevelTestResults ::
  UtteranceLevelTestResults
newUtteranceLevelTestResults =
  UtteranceLevelTestResults' {items = Prelude.mempty}

-- | Contains information about an utterance in the results of the test set
-- execution.
utteranceLevelTestResults_items :: Lens.Lens' UtteranceLevelTestResults [UtteranceLevelTestResultItem]
utteranceLevelTestResults_items = Lens.lens (\UtteranceLevelTestResults' {items} -> items) (\s@UtteranceLevelTestResults' {} a -> s {items = a} :: UtteranceLevelTestResults) Prelude.. Lens.coerced

instance Data.FromJSON UtteranceLevelTestResults where
  parseJSON =
    Data.withObject
      "UtteranceLevelTestResults"
      ( \x ->
          UtteranceLevelTestResults'
            Prelude.<$> (x Data..:? "items" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UtteranceLevelTestResults where
  hashWithSalt _salt UtteranceLevelTestResults' {..} =
    _salt `Prelude.hashWithSalt` items

instance Prelude.NFData UtteranceLevelTestResults where
  rnf UtteranceLevelTestResults' {..} =
    Prelude.rnf items

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
-- Module      : Amazonka.LexV2Models.Types.OverallTestResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.OverallTestResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.OverallTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | Information about the overall test results.
--
-- /See:/ 'newOverallTestResults' smart constructor.
data OverallTestResults = OverallTestResults'
  { -- | A list of the overall test results.
    items :: [OverallTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OverallTestResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'overallTestResults_items' - A list of the overall test results.
newOverallTestResults ::
  OverallTestResults
newOverallTestResults =
  OverallTestResults' {items = Prelude.mempty}

-- | A list of the overall test results.
overallTestResults_items :: Lens.Lens' OverallTestResults [OverallTestResultItem]
overallTestResults_items = Lens.lens (\OverallTestResults' {items} -> items) (\s@OverallTestResults' {} a -> s {items = a} :: OverallTestResults) Prelude.. Lens.coerced

instance Data.FromJSON OverallTestResults where
  parseJSON =
    Data.withObject
      "OverallTestResults"
      ( \x ->
          OverallTestResults'
            Prelude.<$> (x Data..:? "items" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OverallTestResults where
  hashWithSalt _salt OverallTestResults' {..} =
    _salt `Prelude.hashWithSalt` items

instance Prelude.NFData OverallTestResults where
  rnf OverallTestResults' {..} = Prelude.rnf items

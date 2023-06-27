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
-- Module      : Amazonka.Textract.Types.LineItemGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LineItemGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.LineItemFields

-- | A grouping of tables which contain LineItems, with each table identified
-- by the table\'s @LineItemGroupIndex@.
--
-- /See:/ 'newLineItemGroup' smart constructor.
data LineItemGroup = LineItemGroup'
  { -- | The number used to identify a specific table in a document. The first
    -- table encountered will have a LineItemGroupIndex of 1, the second 2,
    -- etc.
    lineItemGroupIndex :: Prelude.Maybe Prelude.Natural,
    -- | The breakdown of information on a particular line of a table.
    lineItems :: Prelude.Maybe [LineItemFields]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineItemGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineItemGroupIndex', 'lineItemGroup_lineItemGroupIndex' - The number used to identify a specific table in a document. The first
-- table encountered will have a LineItemGroupIndex of 1, the second 2,
-- etc.
--
-- 'lineItems', 'lineItemGroup_lineItems' - The breakdown of information on a particular line of a table.
newLineItemGroup ::
  LineItemGroup
newLineItemGroup =
  LineItemGroup'
    { lineItemGroupIndex =
        Prelude.Nothing,
      lineItems = Prelude.Nothing
    }

-- | The number used to identify a specific table in a document. The first
-- table encountered will have a LineItemGroupIndex of 1, the second 2,
-- etc.
lineItemGroup_lineItemGroupIndex :: Lens.Lens' LineItemGroup (Prelude.Maybe Prelude.Natural)
lineItemGroup_lineItemGroupIndex = Lens.lens (\LineItemGroup' {lineItemGroupIndex} -> lineItemGroupIndex) (\s@LineItemGroup' {} a -> s {lineItemGroupIndex = a} :: LineItemGroup)

-- | The breakdown of information on a particular line of a table.
lineItemGroup_lineItems :: Lens.Lens' LineItemGroup (Prelude.Maybe [LineItemFields])
lineItemGroup_lineItems = Lens.lens (\LineItemGroup' {lineItems} -> lineItems) (\s@LineItemGroup' {} a -> s {lineItems = a} :: LineItemGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineItemGroup where
  parseJSON =
    Data.withObject
      "LineItemGroup"
      ( \x ->
          LineItemGroup'
            Prelude.<$> (x Data..:? "LineItemGroupIndex")
            Prelude.<*> (x Data..:? "LineItems" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LineItemGroup where
  hashWithSalt _salt LineItemGroup' {..} =
    _salt
      `Prelude.hashWithSalt` lineItemGroupIndex
      `Prelude.hashWithSalt` lineItems

instance Prelude.NFData LineItemGroup where
  rnf LineItemGroup' {..} =
    Prelude.rnf lineItemGroupIndex
      `Prelude.seq` Prelude.rnf lineItems

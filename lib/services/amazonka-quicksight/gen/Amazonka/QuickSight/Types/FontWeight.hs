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
-- Module      : Amazonka.QuickSight.Types.FontWeight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FontWeight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontWeightName

-- | The option that determines the text display weight, or boldness.
--
-- /See:/ 'newFontWeight' smart constructor.
data FontWeight = FontWeight'
  { -- | The lexical name for the level of boldness of the text display.
    name :: Prelude.Maybe FontWeightName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FontWeight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fontWeight_name' - The lexical name for the level of boldness of the text display.
newFontWeight ::
  FontWeight
newFontWeight = FontWeight' {name = Prelude.Nothing}

-- | The lexical name for the level of boldness of the text display.
fontWeight_name :: Lens.Lens' FontWeight (Prelude.Maybe FontWeightName)
fontWeight_name = Lens.lens (\FontWeight' {name} -> name) (\s@FontWeight' {} a -> s {name = a} :: FontWeight)

instance Data.FromJSON FontWeight where
  parseJSON =
    Data.withObject
      "FontWeight"
      (\x -> FontWeight' Prelude.<$> (x Data..:? "Name"))

instance Prelude.Hashable FontWeight where
  hashWithSalt _salt FontWeight' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData FontWeight where
  rnf FontWeight' {..} = Prelude.rnf name

instance Data.ToJSON FontWeight where
  toJSON FontWeight' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

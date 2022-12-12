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
-- Module      : Amazonka.QuickSight.Types.FontSize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FontSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RelativeFontSize

-- | The option that determines the text display size.
--
-- /See:/ 'newFontSize' smart constructor.
data FontSize = FontSize'
  { -- | The lexical name for the text size, proportional to its surrounding
    -- context.
    relative :: Prelude.Maybe RelativeFontSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FontSize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relative', 'fontSize_relative' - The lexical name for the text size, proportional to its surrounding
-- context.
newFontSize ::
  FontSize
newFontSize = FontSize' {relative = Prelude.Nothing}

-- | The lexical name for the text size, proportional to its surrounding
-- context.
fontSize_relative :: Lens.Lens' FontSize (Prelude.Maybe RelativeFontSize)
fontSize_relative = Lens.lens (\FontSize' {relative} -> relative) (\s@FontSize' {} a -> s {relative = a} :: FontSize)

instance Data.FromJSON FontSize where
  parseJSON =
    Data.withObject
      "FontSize"
      ( \x ->
          FontSize' Prelude.<$> (x Data..:? "Relative")
      )

instance Prelude.Hashable FontSize where
  hashWithSalt _salt FontSize' {..} =
    _salt `Prelude.hashWithSalt` relative

instance Prelude.NFData FontSize where
  rnf FontSize' {..} = Prelude.rnf relative

instance Data.ToJSON FontSize where
  toJSON FontSize' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Relative" Data..=) Prelude.<$> relative]
      )

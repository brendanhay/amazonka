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
-- Module      : Amazonka.QuickSight.Types.DefaultFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DisplayFormat
import Amazonka.QuickSight.Types.DisplayFormatOptions

-- | A structure that represents a default formatting definition.
--
-- /See:/ 'newDefaultFormatting' smart constructor.
data DefaultFormatting = DefaultFormatting'
  { -- | The display format. Valid values for this structure are @AUTO@,
    -- @PERCENT@, @CURRENCY@, @NUMBER@, @DATE@, and @STRING@.
    displayFormat :: Prelude.Maybe DisplayFormat,
    -- | The additional options for display formatting.
    displayFormatOptions :: Prelude.Maybe DisplayFormatOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayFormat', 'defaultFormatting_displayFormat' - The display format. Valid values for this structure are @AUTO@,
-- @PERCENT@, @CURRENCY@, @NUMBER@, @DATE@, and @STRING@.
--
-- 'displayFormatOptions', 'defaultFormatting_displayFormatOptions' - The additional options for display formatting.
newDefaultFormatting ::
  DefaultFormatting
newDefaultFormatting =
  DefaultFormatting'
    { displayFormat = Prelude.Nothing,
      displayFormatOptions = Prelude.Nothing
    }

-- | The display format. Valid values for this structure are @AUTO@,
-- @PERCENT@, @CURRENCY@, @NUMBER@, @DATE@, and @STRING@.
defaultFormatting_displayFormat :: Lens.Lens' DefaultFormatting (Prelude.Maybe DisplayFormat)
defaultFormatting_displayFormat = Lens.lens (\DefaultFormatting' {displayFormat} -> displayFormat) (\s@DefaultFormatting' {} a -> s {displayFormat = a} :: DefaultFormatting)

-- | The additional options for display formatting.
defaultFormatting_displayFormatOptions :: Lens.Lens' DefaultFormatting (Prelude.Maybe DisplayFormatOptions)
defaultFormatting_displayFormatOptions = Lens.lens (\DefaultFormatting' {displayFormatOptions} -> displayFormatOptions) (\s@DefaultFormatting' {} a -> s {displayFormatOptions = a} :: DefaultFormatting)

instance Data.FromJSON DefaultFormatting where
  parseJSON =
    Data.withObject
      "DefaultFormatting"
      ( \x ->
          DefaultFormatting'
            Prelude.<$> (x Data..:? "DisplayFormat")
            Prelude.<*> (x Data..:? "DisplayFormatOptions")
      )

instance Prelude.Hashable DefaultFormatting where
  hashWithSalt _salt DefaultFormatting' {..} =
    _salt
      `Prelude.hashWithSalt` displayFormat
      `Prelude.hashWithSalt` displayFormatOptions

instance Prelude.NFData DefaultFormatting where
  rnf DefaultFormatting' {..} =
    Prelude.rnf displayFormat
      `Prelude.seq` Prelude.rnf displayFormatOptions

instance Data.ToJSON DefaultFormatting where
  toJSON DefaultFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayFormat" Data..=) Prelude.<$> displayFormat,
            ("DisplayFormatOptions" Data..=)
              Prelude.<$> displayFormatOptions
          ]
      )

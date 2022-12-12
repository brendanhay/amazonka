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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingCustomIconOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingCustomIconOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Icon

-- | Custom icon options for an icon set.
--
-- /See:/ 'newConditionalFormattingCustomIconOptions' smart constructor.
data ConditionalFormattingCustomIconOptions = ConditionalFormattingCustomIconOptions'
  { -- | Determines the type of icon.
    icon :: Prelude.Maybe Icon,
    -- | Determines the Unicode icon type.
    unicodeIcon :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingCustomIconOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'icon', 'conditionalFormattingCustomIconOptions_icon' - Determines the type of icon.
--
-- 'unicodeIcon', 'conditionalFormattingCustomIconOptions_unicodeIcon' - Determines the Unicode icon type.
newConditionalFormattingCustomIconOptions ::
  ConditionalFormattingCustomIconOptions
newConditionalFormattingCustomIconOptions =
  ConditionalFormattingCustomIconOptions'
    { icon =
        Prelude.Nothing,
      unicodeIcon = Prelude.Nothing
    }

-- | Determines the type of icon.
conditionalFormattingCustomIconOptions_icon :: Lens.Lens' ConditionalFormattingCustomIconOptions (Prelude.Maybe Icon)
conditionalFormattingCustomIconOptions_icon = Lens.lens (\ConditionalFormattingCustomIconOptions' {icon} -> icon) (\s@ConditionalFormattingCustomIconOptions' {} a -> s {icon = a} :: ConditionalFormattingCustomIconOptions)

-- | Determines the Unicode icon type.
conditionalFormattingCustomIconOptions_unicodeIcon :: Lens.Lens' ConditionalFormattingCustomIconOptions (Prelude.Maybe Prelude.Text)
conditionalFormattingCustomIconOptions_unicodeIcon = Lens.lens (\ConditionalFormattingCustomIconOptions' {unicodeIcon} -> unicodeIcon) (\s@ConditionalFormattingCustomIconOptions' {} a -> s {unicodeIcon = a} :: ConditionalFormattingCustomIconOptions)

instance
  Data.FromJSON
    ConditionalFormattingCustomIconOptions
  where
  parseJSON =
    Data.withObject
      "ConditionalFormattingCustomIconOptions"
      ( \x ->
          ConditionalFormattingCustomIconOptions'
            Prelude.<$> (x Data..:? "Icon")
            Prelude.<*> (x Data..:? "UnicodeIcon")
      )

instance
  Prelude.Hashable
    ConditionalFormattingCustomIconOptions
  where
  hashWithSalt
    _salt
    ConditionalFormattingCustomIconOptions' {..} =
      _salt `Prelude.hashWithSalt` icon
        `Prelude.hashWithSalt` unicodeIcon

instance
  Prelude.NFData
    ConditionalFormattingCustomIconOptions
  where
  rnf ConditionalFormattingCustomIconOptions' {..} =
    Prelude.rnf icon
      `Prelude.seq` Prelude.rnf unicodeIcon

instance
  Data.ToJSON
    ConditionalFormattingCustomIconOptions
  where
  toJSON ConditionalFormattingCustomIconOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Icon" Data..=) Prelude.<$> icon,
            ("UnicodeIcon" Data..=) Prelude.<$> unicodeIcon
          ]
      )

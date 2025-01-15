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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingIcon
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingIcon where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingCustomIconCondition
import Amazonka.QuickSight.Types.ConditionalFormattingIconSet

-- | The formatting configuration for the icon.
--
-- /See:/ 'newConditionalFormattingIcon' smart constructor.
data ConditionalFormattingIcon = ConditionalFormattingIcon'
  { -- | Determines the custom condition for an icon set.
    customCondition :: Prelude.Maybe ConditionalFormattingCustomIconCondition,
    -- | Formatting configuration for icon set.
    iconSet :: Prelude.Maybe ConditionalFormattingIconSet
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingIcon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customCondition', 'conditionalFormattingIcon_customCondition' - Determines the custom condition for an icon set.
--
-- 'iconSet', 'conditionalFormattingIcon_iconSet' - Formatting configuration for icon set.
newConditionalFormattingIcon ::
  ConditionalFormattingIcon
newConditionalFormattingIcon =
  ConditionalFormattingIcon'
    { customCondition =
        Prelude.Nothing,
      iconSet = Prelude.Nothing
    }

-- | Determines the custom condition for an icon set.
conditionalFormattingIcon_customCondition :: Lens.Lens' ConditionalFormattingIcon (Prelude.Maybe ConditionalFormattingCustomIconCondition)
conditionalFormattingIcon_customCondition = Lens.lens (\ConditionalFormattingIcon' {customCondition} -> customCondition) (\s@ConditionalFormattingIcon' {} a -> s {customCondition = a} :: ConditionalFormattingIcon)

-- | Formatting configuration for icon set.
conditionalFormattingIcon_iconSet :: Lens.Lens' ConditionalFormattingIcon (Prelude.Maybe ConditionalFormattingIconSet)
conditionalFormattingIcon_iconSet = Lens.lens (\ConditionalFormattingIcon' {iconSet} -> iconSet) (\s@ConditionalFormattingIcon' {} a -> s {iconSet = a} :: ConditionalFormattingIcon)

instance Data.FromJSON ConditionalFormattingIcon where
  parseJSON =
    Data.withObject
      "ConditionalFormattingIcon"
      ( \x ->
          ConditionalFormattingIcon'
            Prelude.<$> (x Data..:? "CustomCondition")
            Prelude.<*> (x Data..:? "IconSet")
      )

instance Prelude.Hashable ConditionalFormattingIcon where
  hashWithSalt _salt ConditionalFormattingIcon' {..} =
    _salt
      `Prelude.hashWithSalt` customCondition
      `Prelude.hashWithSalt` iconSet

instance Prelude.NFData ConditionalFormattingIcon where
  rnf ConditionalFormattingIcon' {..} =
    Prelude.rnf customCondition `Prelude.seq`
      Prelude.rnf iconSet

instance Data.ToJSON ConditionalFormattingIcon where
  toJSON ConditionalFormattingIcon' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomCondition" Data..=)
              Prelude.<$> customCondition,
            ("IconSet" Data..=) Prelude.<$> iconSet
          ]
      )

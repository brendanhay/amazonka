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
-- Module      : Amazonka.QuickSight.Types.AccountCustomization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AccountCustomization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon QuickSight customizations associated with your Amazon Web
-- Services account or a Amazon QuickSight namespace in a specific Amazon
-- Web Services Region.
--
-- /See:/ 'newAccountCustomization' smart constructor.
data AccountCustomization = AccountCustomization'
  { -- | The default theme for this Amazon QuickSight subscription.
    defaultTheme :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountCustomization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTheme', 'accountCustomization_defaultTheme' - The default theme for this Amazon QuickSight subscription.
newAccountCustomization ::
  AccountCustomization
newAccountCustomization =
  AccountCustomization'
    { defaultTheme =
        Prelude.Nothing
    }

-- | The default theme for this Amazon QuickSight subscription.
accountCustomization_defaultTheme :: Lens.Lens' AccountCustomization (Prelude.Maybe Prelude.Text)
accountCustomization_defaultTheme = Lens.lens (\AccountCustomization' {defaultTheme} -> defaultTheme) (\s@AccountCustomization' {} a -> s {defaultTheme = a} :: AccountCustomization)

instance Core.FromJSON AccountCustomization where
  parseJSON =
    Core.withObject
      "AccountCustomization"
      ( \x ->
          AccountCustomization'
            Prelude.<$> (x Core..:? "DefaultTheme")
      )

instance Prelude.Hashable AccountCustomization

instance Prelude.NFData AccountCustomization

instance Core.ToJSON AccountCustomization where
  toJSON AccountCustomization' {..} =
    Core.object
      ( Prelude.catMaybes
          [("DefaultTheme" Core..=) Prelude.<$> defaultTheme]
      )

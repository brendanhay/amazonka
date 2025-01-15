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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AccountCustomization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon QuickSight customizations associated with your Amazon Web
-- Services account or a QuickSight namespace in a specific Amazon Web
-- Services Region.
--
-- /See:/ 'newAccountCustomization' smart constructor.
data AccountCustomization = AccountCustomization'
  { -- | The default email customization template.
    defaultEmailCustomizationTemplate :: Prelude.Maybe Prelude.Text,
    -- | The default theme for this Amazon QuickSight subscription.
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
-- 'defaultEmailCustomizationTemplate', 'accountCustomization_defaultEmailCustomizationTemplate' - The default email customization template.
--
-- 'defaultTheme', 'accountCustomization_defaultTheme' - The default theme for this Amazon QuickSight subscription.
newAccountCustomization ::
  AccountCustomization
newAccountCustomization =
  AccountCustomization'
    { defaultEmailCustomizationTemplate =
        Prelude.Nothing,
      defaultTheme = Prelude.Nothing
    }

-- | The default email customization template.
accountCustomization_defaultEmailCustomizationTemplate :: Lens.Lens' AccountCustomization (Prelude.Maybe Prelude.Text)
accountCustomization_defaultEmailCustomizationTemplate = Lens.lens (\AccountCustomization' {defaultEmailCustomizationTemplate} -> defaultEmailCustomizationTemplate) (\s@AccountCustomization' {} a -> s {defaultEmailCustomizationTemplate = a} :: AccountCustomization)

-- | The default theme for this Amazon QuickSight subscription.
accountCustomization_defaultTheme :: Lens.Lens' AccountCustomization (Prelude.Maybe Prelude.Text)
accountCustomization_defaultTheme = Lens.lens (\AccountCustomization' {defaultTheme} -> defaultTheme) (\s@AccountCustomization' {} a -> s {defaultTheme = a} :: AccountCustomization)

instance Data.FromJSON AccountCustomization where
  parseJSON =
    Data.withObject
      "AccountCustomization"
      ( \x ->
          AccountCustomization'
            Prelude.<$> (x Data..:? "DefaultEmailCustomizationTemplate")
            Prelude.<*> (x Data..:? "DefaultTheme")
      )

instance Prelude.Hashable AccountCustomization where
  hashWithSalt _salt AccountCustomization' {..} =
    _salt
      `Prelude.hashWithSalt` defaultEmailCustomizationTemplate
      `Prelude.hashWithSalt` defaultTheme

instance Prelude.NFData AccountCustomization where
  rnf AccountCustomization' {..} =
    Prelude.rnf defaultEmailCustomizationTemplate `Prelude.seq`
      Prelude.rnf defaultTheme

instance Data.ToJSON AccountCustomization where
  toJSON AccountCustomization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultEmailCustomizationTemplate" Data..=)
              Prelude.<$> defaultEmailCustomizationTemplate,
            ("DefaultTheme" Data..=) Prelude.<$> defaultTheme
          ]
      )

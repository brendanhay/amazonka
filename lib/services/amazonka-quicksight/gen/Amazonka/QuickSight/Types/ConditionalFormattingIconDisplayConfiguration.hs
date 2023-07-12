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
-- Module      : Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingIconDisplayOption

-- | Determines the icon display configuration.
--
-- /See:/ 'newConditionalFormattingIconDisplayConfiguration' smart constructor.
data ConditionalFormattingIconDisplayConfiguration = ConditionalFormattingIconDisplayConfiguration'
  { -- | Determines the icon display configuration.
    iconDisplayOption :: Prelude.Maybe ConditionalFormattingIconDisplayOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionalFormattingIconDisplayConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iconDisplayOption', 'conditionalFormattingIconDisplayConfiguration_iconDisplayOption' - Determines the icon display configuration.
newConditionalFormattingIconDisplayConfiguration ::
  ConditionalFormattingIconDisplayConfiguration
newConditionalFormattingIconDisplayConfiguration =
  ConditionalFormattingIconDisplayConfiguration'
    { iconDisplayOption =
        Prelude.Nothing
    }

-- | Determines the icon display configuration.
conditionalFormattingIconDisplayConfiguration_iconDisplayOption :: Lens.Lens' ConditionalFormattingIconDisplayConfiguration (Prelude.Maybe ConditionalFormattingIconDisplayOption)
conditionalFormattingIconDisplayConfiguration_iconDisplayOption = Lens.lens (\ConditionalFormattingIconDisplayConfiguration' {iconDisplayOption} -> iconDisplayOption) (\s@ConditionalFormattingIconDisplayConfiguration' {} a -> s {iconDisplayOption = a} :: ConditionalFormattingIconDisplayConfiguration)

instance
  Data.FromJSON
    ConditionalFormattingIconDisplayConfiguration
  where
  parseJSON =
    Data.withObject
      "ConditionalFormattingIconDisplayConfiguration"
      ( \x ->
          ConditionalFormattingIconDisplayConfiguration'
            Prelude.<$> (x Data..:? "IconDisplayOption")
      )

instance
  Prelude.Hashable
    ConditionalFormattingIconDisplayConfiguration
  where
  hashWithSalt
    _salt
    ConditionalFormattingIconDisplayConfiguration' {..} =
      _salt `Prelude.hashWithSalt` iconDisplayOption

instance
  Prelude.NFData
    ConditionalFormattingIconDisplayConfiguration
  where
  rnf
    ConditionalFormattingIconDisplayConfiguration' {..} =
      Prelude.rnf iconDisplayOption

instance
  Data.ToJSON
    ConditionalFormattingIconDisplayConfiguration
  where
  toJSON
    ConditionalFormattingIconDisplayConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("IconDisplayOption" Data..=)
                Prelude.<$> iconDisplayOption
            ]
        )

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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineStyleConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineStyleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ReferenceLinePatternType

-- | The style configuration of the reference line.
--
-- /See:/ 'newReferenceLineStyleConfiguration' smart constructor.
data ReferenceLineStyleConfiguration = ReferenceLineStyleConfiguration'
  { -- | The hex color of the reference line.
    color :: Prelude.Maybe Prelude.Text,
    -- | The pattern type of the line style. Choose one of the following options:
    --
    -- -   @SOLID@
    --
    -- -   @DASHED@
    --
    -- -   @DOTTED@
    pattern' :: Prelude.Maybe ReferenceLinePatternType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineStyleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'referenceLineStyleConfiguration_color' - The hex color of the reference line.
--
-- 'pattern'', 'referenceLineStyleConfiguration_pattern' - The pattern type of the line style. Choose one of the following options:
--
-- -   @SOLID@
--
-- -   @DASHED@
--
-- -   @DOTTED@
newReferenceLineStyleConfiguration ::
  ReferenceLineStyleConfiguration
newReferenceLineStyleConfiguration =
  ReferenceLineStyleConfiguration'
    { color =
        Prelude.Nothing,
      pattern' = Prelude.Nothing
    }

-- | The hex color of the reference line.
referenceLineStyleConfiguration_color :: Lens.Lens' ReferenceLineStyleConfiguration (Prelude.Maybe Prelude.Text)
referenceLineStyleConfiguration_color = Lens.lens (\ReferenceLineStyleConfiguration' {color} -> color) (\s@ReferenceLineStyleConfiguration' {} a -> s {color = a} :: ReferenceLineStyleConfiguration)

-- | The pattern type of the line style. Choose one of the following options:
--
-- -   @SOLID@
--
-- -   @DASHED@
--
-- -   @DOTTED@
referenceLineStyleConfiguration_pattern :: Lens.Lens' ReferenceLineStyleConfiguration (Prelude.Maybe ReferenceLinePatternType)
referenceLineStyleConfiguration_pattern = Lens.lens (\ReferenceLineStyleConfiguration' {pattern'} -> pattern') (\s@ReferenceLineStyleConfiguration' {} a -> s {pattern' = a} :: ReferenceLineStyleConfiguration)

instance
  Data.FromJSON
    ReferenceLineStyleConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineStyleConfiguration"
      ( \x ->
          ReferenceLineStyleConfiguration'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "Pattern")
      )

instance
  Prelude.Hashable
    ReferenceLineStyleConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineStyleConfiguration' {..} =
      _salt `Prelude.hashWithSalt` color
        `Prelude.hashWithSalt` pattern'

instance
  Prelude.NFData
    ReferenceLineStyleConfiguration
  where
  rnf ReferenceLineStyleConfiguration' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf pattern'

instance Data.ToJSON ReferenceLineStyleConfiguration where
  toJSON ReferenceLineStyleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Color" Data..=) Prelude.<$> color,
            ("Pattern" Data..=) Prelude.<$> pattern'
          ]
      )

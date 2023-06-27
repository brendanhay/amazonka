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
-- Module      : Amazonka.QuickSight.Types.NegativeFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NegativeFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents a negative format.
--
-- /See:/ 'newNegativeFormat' smart constructor.
data NegativeFormat = NegativeFormat'
  { -- | The prefix for a negative format.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The suffix for a negative format.
    suffix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NegativeFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'negativeFormat_prefix' - The prefix for a negative format.
--
-- 'suffix', 'negativeFormat_suffix' - The suffix for a negative format.
newNegativeFormat ::
  NegativeFormat
newNegativeFormat =
  NegativeFormat'
    { prefix = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The prefix for a negative format.
negativeFormat_prefix :: Lens.Lens' NegativeFormat (Prelude.Maybe Prelude.Text)
negativeFormat_prefix = Lens.lens (\NegativeFormat' {prefix} -> prefix) (\s@NegativeFormat' {} a -> s {prefix = a} :: NegativeFormat)

-- | The suffix for a negative format.
negativeFormat_suffix :: Lens.Lens' NegativeFormat (Prelude.Maybe Prelude.Text)
negativeFormat_suffix = Lens.lens (\NegativeFormat' {suffix} -> suffix) (\s@NegativeFormat' {} a -> s {suffix = a} :: NegativeFormat)

instance Data.FromJSON NegativeFormat where
  parseJSON =
    Data.withObject
      "NegativeFormat"
      ( \x ->
          NegativeFormat'
            Prelude.<$> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "Suffix")
      )

instance Prelude.Hashable NegativeFormat where
  hashWithSalt _salt NegativeFormat' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData NegativeFormat where
  rnf NegativeFormat' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON NegativeFormat where
  toJSON NegativeFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Prefix" Data..=) Prelude.<$> prefix,
            ("Suffix" Data..=) Prelude.<$> suffix
          ]
      )

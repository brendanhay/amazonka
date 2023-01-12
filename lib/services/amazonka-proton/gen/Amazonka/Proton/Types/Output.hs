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
-- Module      : Amazonka.Proton.Types.Output
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An infrastructure as code defined resource output.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The output key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The output value.
    valueString :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'output_key' - The output key.
--
-- 'valueString', 'output_valueString' - The output value.
newOutput ::
  Output
newOutput =
  Output'
    { key = Prelude.Nothing,
      valueString = Prelude.Nothing
    }

-- | The output key.
output_key :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_key = Lens.lens (\Output' {key} -> key) (\s@Output' {} a -> s {key = a} :: Output)

-- | The output value.
output_valueString :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_valueString = Lens.lens (\Output' {valueString} -> valueString) (\s@Output' {} a -> s {valueString = a} :: Output)

instance Data.FromJSON Output where
  parseJSON =
    Data.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "valueString")
      )

instance Prelude.Hashable Output where
  hashWithSalt _salt Output' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` valueString

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf valueString

instance Data.ToJSON Output where
  toJSON Output' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("valueString" Data..=) Prelude.<$> valueString
          ]
      )

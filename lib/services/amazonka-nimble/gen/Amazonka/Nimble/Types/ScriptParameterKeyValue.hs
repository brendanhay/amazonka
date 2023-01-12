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
-- Module      : Amazonka.Nimble.Types.ScriptParameterKeyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.ScriptParameterKeyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A parameter for a studio component script, in the form of a key-value
-- pair.
--
-- /See:/ 'newScriptParameterKeyValue' smart constructor.
data ScriptParameterKeyValue = ScriptParameterKeyValue'
  { -- | A script parameter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | A script parameter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScriptParameterKeyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'scriptParameterKeyValue_key' - A script parameter key.
--
-- 'value', 'scriptParameterKeyValue_value' - A script parameter value.
newScriptParameterKeyValue ::
  ScriptParameterKeyValue
newScriptParameterKeyValue =
  ScriptParameterKeyValue'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A script parameter key.
scriptParameterKeyValue_key :: Lens.Lens' ScriptParameterKeyValue (Prelude.Maybe Prelude.Text)
scriptParameterKeyValue_key = Lens.lens (\ScriptParameterKeyValue' {key} -> key) (\s@ScriptParameterKeyValue' {} a -> s {key = a} :: ScriptParameterKeyValue)

-- | A script parameter value.
scriptParameterKeyValue_value :: Lens.Lens' ScriptParameterKeyValue (Prelude.Maybe Prelude.Text)
scriptParameterKeyValue_value = Lens.lens (\ScriptParameterKeyValue' {value} -> value) (\s@ScriptParameterKeyValue' {} a -> s {value = a} :: ScriptParameterKeyValue)

instance Data.FromJSON ScriptParameterKeyValue where
  parseJSON =
    Data.withObject
      "ScriptParameterKeyValue"
      ( \x ->
          ScriptParameterKeyValue'
            Prelude.<$> (x Data..:? "key") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ScriptParameterKeyValue where
  hashWithSalt _salt ScriptParameterKeyValue' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ScriptParameterKeyValue where
  rnf ScriptParameterKeyValue' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ScriptParameterKeyValue where
  toJSON ScriptParameterKeyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )

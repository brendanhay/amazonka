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
-- Module      : Amazonka.AppMesh.Types.HeaderMatchMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HeaderMatchMethod where

import Amazonka.AppMesh.Types.MatchRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the method and value to match with the header
-- value sent in a request. Specify one match method.
--
-- /See:/ 'newHeaderMatchMethod' smart constructor.
data HeaderMatchMethod = HeaderMatchMethod'
  { -- | The value sent by the client must match the specified value exactly.
    exact :: Prelude.Maybe Prelude.Text,
    -- | The value sent by the client must begin with the specified characters.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | An object that represents the range of values to match on.
    range :: Prelude.Maybe MatchRange,
    -- | The value sent by the client must include the specified characters.
    regex :: Prelude.Maybe Prelude.Text,
    -- | The value sent by the client must end with the specified characters.
    suffix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderMatchMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'headerMatchMethod_exact' - The value sent by the client must match the specified value exactly.
--
-- 'prefix', 'headerMatchMethod_prefix' - The value sent by the client must begin with the specified characters.
--
-- 'range', 'headerMatchMethod_range' - An object that represents the range of values to match on.
--
-- 'regex', 'headerMatchMethod_regex' - The value sent by the client must include the specified characters.
--
-- 'suffix', 'headerMatchMethod_suffix' - The value sent by the client must end with the specified characters.
newHeaderMatchMethod ::
  HeaderMatchMethod
newHeaderMatchMethod =
  HeaderMatchMethod'
    { exact = Prelude.Nothing,
      prefix = Prelude.Nothing,
      range = Prelude.Nothing,
      regex = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The value sent by the client must match the specified value exactly.
headerMatchMethod_exact :: Lens.Lens' HeaderMatchMethod (Prelude.Maybe Prelude.Text)
headerMatchMethod_exact = Lens.lens (\HeaderMatchMethod' {exact} -> exact) (\s@HeaderMatchMethod' {} a -> s {exact = a} :: HeaderMatchMethod)

-- | The value sent by the client must begin with the specified characters.
headerMatchMethod_prefix :: Lens.Lens' HeaderMatchMethod (Prelude.Maybe Prelude.Text)
headerMatchMethod_prefix = Lens.lens (\HeaderMatchMethod' {prefix} -> prefix) (\s@HeaderMatchMethod' {} a -> s {prefix = a} :: HeaderMatchMethod)

-- | An object that represents the range of values to match on.
headerMatchMethod_range :: Lens.Lens' HeaderMatchMethod (Prelude.Maybe MatchRange)
headerMatchMethod_range = Lens.lens (\HeaderMatchMethod' {range} -> range) (\s@HeaderMatchMethod' {} a -> s {range = a} :: HeaderMatchMethod)

-- | The value sent by the client must include the specified characters.
headerMatchMethod_regex :: Lens.Lens' HeaderMatchMethod (Prelude.Maybe Prelude.Text)
headerMatchMethod_regex = Lens.lens (\HeaderMatchMethod' {regex} -> regex) (\s@HeaderMatchMethod' {} a -> s {regex = a} :: HeaderMatchMethod)

-- | The value sent by the client must end with the specified characters.
headerMatchMethod_suffix :: Lens.Lens' HeaderMatchMethod (Prelude.Maybe Prelude.Text)
headerMatchMethod_suffix = Lens.lens (\HeaderMatchMethod' {suffix} -> suffix) (\s@HeaderMatchMethod' {} a -> s {suffix = a} :: HeaderMatchMethod)

instance Data.FromJSON HeaderMatchMethod where
  parseJSON =
    Data.withObject
      "HeaderMatchMethod"
      ( \x ->
          HeaderMatchMethod'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "range")
            Prelude.<*> (x Data..:? "regex")
            Prelude.<*> (x Data..:? "suffix")
      )

instance Prelude.Hashable HeaderMatchMethod where
  hashWithSalt _salt HeaderMatchMethod' {..} =
    _salt
      `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData HeaderMatchMethod where
  rnf HeaderMatchMethod' {..} =
    Prelude.rnf exact
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf regex
      `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON HeaderMatchMethod where
  toJSON HeaderMatchMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("range" Data..=) Prelude.<$> range,
            ("regex" Data..=) Prelude.<$> regex,
            ("suffix" Data..=) Prelude.<$> suffix
          ]
      )

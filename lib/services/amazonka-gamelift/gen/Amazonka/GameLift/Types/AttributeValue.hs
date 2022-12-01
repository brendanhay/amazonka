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
-- Module      : Amazonka.GameLift.Types.AttributeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.AttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Values for use in Player attribute key-value pairs. This object lets you
-- specify an attribute value using any of the valid data types: string,
-- number, string array, or data map. Each @AttributeValue@ object can use
-- only one of the available properties.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | For a list of up to 100 strings. Maximum length for each string is 100
    -- characters. Duplicate values are not recognized; all occurrences of the
    -- repeated value after the first of a repeated value are ignored.
    sl :: Prelude.Maybe [Prelude.Text],
    -- | For single string values. Maximum string length is 100 characters.
    s :: Prelude.Maybe Prelude.Text,
    -- | For a map of up to 10 data type:value pairs. Maximum length for each
    -- string value is 100 characters.
    sdm :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | For number values, expressed as double.
    n :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sl', 'attributeValue_sl' - For a list of up to 100 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
--
-- 's', 'attributeValue_s' - For single string values. Maximum string length is 100 characters.
--
-- 'sdm', 'attributeValue_sdm' - For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
--
-- 'n', 'attributeValue_n' - For number values, expressed as double.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { sl = Prelude.Nothing,
      s = Prelude.Nothing,
      sdm = Prelude.Nothing,
      n = Prelude.Nothing
    }

-- | For a list of up to 100 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
attributeValue_sl :: Lens.Lens' AttributeValue (Prelude.Maybe [Prelude.Text])
attributeValue_sl = Lens.lens (\AttributeValue' {sl} -> sl) (\s@AttributeValue' {} a -> s {sl = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | For single string values. Maximum string length is 100 characters.
attributeValue_s :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_s = Lens.lens (\AttributeValue' {s} -> s) (\s@AttributeValue' {} a -> s {s = a} :: AttributeValue)

-- | For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
attributeValue_sdm :: Lens.Lens' AttributeValue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
attributeValue_sdm = Lens.lens (\AttributeValue' {sdm} -> sdm) (\s@AttributeValue' {} a -> s {sdm = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | For number values, expressed as double.
attributeValue_n :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Double)
attributeValue_n = Lens.lens (\AttributeValue' {n} -> n) (\s@AttributeValue' {} a -> s {n = a} :: AttributeValue)

instance Core.FromJSON AttributeValue where
  parseJSON =
    Core.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Prelude.<$> (x Core..:? "SL" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "S")
            Prelude.<*> (x Core..:? "SDM" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "N")
      )

instance Prelude.Hashable AttributeValue where
  hashWithSalt _salt AttributeValue' {..} =
    _salt `Prelude.hashWithSalt` sl
      `Prelude.hashWithSalt` s
      `Prelude.hashWithSalt` sdm
      `Prelude.hashWithSalt` n

instance Prelude.NFData AttributeValue where
  rnf AttributeValue' {..} =
    Prelude.rnf sl
      `Prelude.seq` Prelude.rnf s
      `Prelude.seq` Prelude.rnf sdm
      `Prelude.seq` Prelude.rnf n

instance Core.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SL" Core..=) Prelude.<$> sl,
            ("S" Core..=) Prelude.<$> s,
            ("SDM" Core..=) Prelude.<$> sdm,
            ("N" Core..=) Prelude.<$> n
          ]
      )

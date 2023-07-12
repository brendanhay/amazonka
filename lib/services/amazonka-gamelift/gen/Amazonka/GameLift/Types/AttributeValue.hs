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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.AttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Values for use in player attribute key-value pairs. This object lets you
-- specify an attribute value using any of the valid data types: string,
-- number, string array, or data map. Each @AttributeValue@ object can use
-- only one of the available properties.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | For number values, expressed as double.
    n :: Prelude.Maybe Prelude.Double,
    -- | For single string values. Maximum string length is 100 characters.
    s :: Prelude.Maybe Prelude.Text,
    -- | For a map of up to 10 data type:value pairs. Maximum length for each
    -- string value is 100 characters.
    sdm :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | For a list of up to 100 strings. Maximum length for each string is 100
    -- characters. Duplicate values are not recognized; all occurrences of the
    -- repeated value after the first of a repeated value are ignored.
    sl :: Prelude.Maybe [Prelude.Text]
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
-- 'n', 'attributeValue_n' - For number values, expressed as double.
--
-- 's', 'attributeValue_s' - For single string values. Maximum string length is 100 characters.
--
-- 'sdm', 'attributeValue_sdm' - For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
--
-- 'sl', 'attributeValue_sl' - For a list of up to 100 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { n = Prelude.Nothing,
      s = Prelude.Nothing,
      sdm = Prelude.Nothing,
      sl = Prelude.Nothing
    }

-- | For number values, expressed as double.
attributeValue_n :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Double)
attributeValue_n = Lens.lens (\AttributeValue' {n} -> n) (\s@AttributeValue' {} a -> s {n = a} :: AttributeValue)

-- | For single string values. Maximum string length is 100 characters.
attributeValue_s :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_s = Lens.lens (\AttributeValue' {s} -> s) (\s@AttributeValue' {} a -> s {s = a} :: AttributeValue)

-- | For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
attributeValue_sdm :: Lens.Lens' AttributeValue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
attributeValue_sdm = Lens.lens (\AttributeValue' {sdm} -> sdm) (\s@AttributeValue' {} a -> s {sdm = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | For a list of up to 100 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
attributeValue_sl :: Lens.Lens' AttributeValue (Prelude.Maybe [Prelude.Text])
attributeValue_sl = Lens.lens (\AttributeValue' {sl} -> sl) (\s@AttributeValue' {} a -> s {sl = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AttributeValue where
  parseJSON =
    Data.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Prelude.<$> (x Data..:? "N")
            Prelude.<*> (x Data..:? "S")
            Prelude.<*> (x Data..:? "SDM" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SL" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AttributeValue where
  hashWithSalt _salt AttributeValue' {..} =
    _salt
      `Prelude.hashWithSalt` n
      `Prelude.hashWithSalt` s
      `Prelude.hashWithSalt` sdm
      `Prelude.hashWithSalt` sl

instance Prelude.NFData AttributeValue where
  rnf AttributeValue' {..} =
    Prelude.rnf n
      `Prelude.seq` Prelude.rnf s
      `Prelude.seq` Prelude.rnf sdm
      `Prelude.seq` Prelude.rnf sl

instance Data.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("N" Data..=) Prelude.<$> n,
            ("S" Data..=) Prelude.<$> s,
            ("SDM" Data..=) Prelude.<$> sdm,
            ("SL" Data..=) Prelude.<$> sl
          ]
      )

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
-- Module      : Network.AWS.GameLift.Types.AttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Values for use in Player attribute key-value pairs. This object lets you
-- specify an attribute value using any of the valid data types: string,
-- number, string array, or data map. Each @AttributeValue@ object can use
-- only one of the available properties.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | For a list of up to 10 strings. Maximum length for each string is 100
    -- characters. Duplicate values are not recognized; all occurrences of the
    -- repeated value after the first of a repeated value are ignored.
    sl :: Core.Maybe [Core.Text],
    -- | For number values, expressed as double.
    n :: Core.Maybe Core.Double,
    -- | For single string values. Maximum string length is 100 characters.
    s :: Core.Maybe Core.Text,
    -- | For a map of up to 10 data type:value pairs. Maximum length for each
    -- string value is 100 characters.
    sdm :: Core.Maybe (Core.HashMap Core.Text Core.Double)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sl', 'attributeValue_sl' - For a list of up to 10 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
--
-- 'n', 'attributeValue_n' - For number values, expressed as double.
--
-- 's', 'attributeValue_s' - For single string values. Maximum string length is 100 characters.
--
-- 'sdm', 'attributeValue_sdm' - For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { sl = Core.Nothing,
      n = Core.Nothing,
      s = Core.Nothing,
      sdm = Core.Nothing
    }

-- | For a list of up to 10 strings. Maximum length for each string is 100
-- characters. Duplicate values are not recognized; all occurrences of the
-- repeated value after the first of a repeated value are ignored.
attributeValue_sl :: Lens.Lens' AttributeValue (Core.Maybe [Core.Text])
attributeValue_sl = Lens.lens (\AttributeValue' {sl} -> sl) (\s@AttributeValue' {} a -> s {sl = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

-- | For number values, expressed as double.
attributeValue_n :: Lens.Lens' AttributeValue (Core.Maybe Core.Double)
attributeValue_n = Lens.lens (\AttributeValue' {n} -> n) (\s@AttributeValue' {} a -> s {n = a} :: AttributeValue)

-- | For single string values. Maximum string length is 100 characters.
attributeValue_s :: Lens.Lens' AttributeValue (Core.Maybe Core.Text)
attributeValue_s = Lens.lens (\AttributeValue' {s} -> s) (\s@AttributeValue' {} a -> s {s = a} :: AttributeValue)

-- | For a map of up to 10 data type:value pairs. Maximum length for each
-- string value is 100 characters.
attributeValue_sdm :: Lens.Lens' AttributeValue (Core.Maybe (Core.HashMap Core.Text Core.Double))
attributeValue_sdm = Lens.lens (\AttributeValue' {sdm} -> sdm) (\s@AttributeValue' {} a -> s {sdm = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AttributeValue where
  parseJSON =
    Core.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Core.<$> (x Core..:? "SL" Core..!= Core.mempty)
            Core.<*> (x Core..:? "N")
            Core.<*> (x Core..:? "S")
            Core.<*> (x Core..:? "SDM" Core..!= Core.mempty)
      )

instance Core.Hashable AttributeValue

instance Core.NFData AttributeValue

instance Core.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SL" Core..=) Core.<$> sl,
            ("N" Core..=) Core.<$> n,
            ("S" Core..=) Core.<$> s,
            ("SDM" Core..=) Core.<$> sdm
          ]
      )

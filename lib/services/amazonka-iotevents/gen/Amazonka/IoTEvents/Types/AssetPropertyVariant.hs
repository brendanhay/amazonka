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
-- Module      : Amazonka.IoTEvents.Types.AssetPropertyVariant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AssetPropertyVariant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains an asset property value. For more information,
-- see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_Variant.html Variant>
-- in the /AWS IoT SiteWise API Reference/.
--
-- You must use expressions for all parameters in @AssetPropertyVariant@.
-- The expressions accept literals, operators, functions, references, and
-- substitution templates.
--
-- __Examples__
--
-- -   For literal values, the expressions must contain single quotes. For
--     example, the value for the @integerValue@ parameter can be
--     @\'100\'@.
--
-- -   For references, you must specify either variables or parameters. For
--     example, the value for the @booleanValue@ parameter can be
--     @$variable.offline@.
--
-- -   For a substitution template, you must use @${}@, and the template
--     must be in single quotes. A substitution template can also contain a
--     combination of literals, operators, functions, references, and
--     substitution templates.
--
--     In the following example, the value for the @doubleValue@ parameter
--     uses a substitution template.
--
--     @\'${$input.TemperatureInput.sensorData.temperature * 6 \/ 5 + 32}\'@
--
-- For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-expressions.html Expressions>
-- in the /AWS IoT Events Developer Guide/.
--
-- You must specify one of the following value types, depending on the
-- @dataType@ of the specified asset property. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_AssetProperty.html AssetProperty>
-- in the /AWS IoT SiteWise API Reference/.
--
-- /See:/ 'newAssetPropertyVariant' smart constructor.
data AssetPropertyVariant = AssetPropertyVariant'
  { -- | The asset property value is an integer. You must use an expression, and
    -- the evaluated result should be an integer.
    integerValue :: Prelude.Maybe Prelude.Text,
    -- | The asset property value is a double. You must use an expression, and
    -- the evaluated result should be a double.
    doubleValue :: Prelude.Maybe Prelude.Text,
    -- | The asset property value is a Boolean value that must be @\'TRUE\'@ or
    -- @\'FALSE\'@. You must use an expression, and the evaluated result should
    -- be a Boolean value.
    booleanValue :: Prelude.Maybe Prelude.Text,
    -- | The asset property value is a string. You must use an expression, and
    -- the evaluated result should be a string.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetPropertyVariant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'assetPropertyVariant_integerValue' - The asset property value is an integer. You must use an expression, and
-- the evaluated result should be an integer.
--
-- 'doubleValue', 'assetPropertyVariant_doubleValue' - The asset property value is a double. You must use an expression, and
-- the evaluated result should be a double.
--
-- 'booleanValue', 'assetPropertyVariant_booleanValue' - The asset property value is a Boolean value that must be @\'TRUE\'@ or
-- @\'FALSE\'@. You must use an expression, and the evaluated result should
-- be a Boolean value.
--
-- 'stringValue', 'assetPropertyVariant_stringValue' - The asset property value is a string. You must use an expression, and
-- the evaluated result should be a string.
newAssetPropertyVariant ::
  AssetPropertyVariant
newAssetPropertyVariant =
  AssetPropertyVariant'
    { integerValue =
        Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The asset property value is an integer. You must use an expression, and
-- the evaluated result should be an integer.
assetPropertyVariant_integerValue :: Lens.Lens' AssetPropertyVariant (Prelude.Maybe Prelude.Text)
assetPropertyVariant_integerValue = Lens.lens (\AssetPropertyVariant' {integerValue} -> integerValue) (\s@AssetPropertyVariant' {} a -> s {integerValue = a} :: AssetPropertyVariant)

-- | The asset property value is a double. You must use an expression, and
-- the evaluated result should be a double.
assetPropertyVariant_doubleValue :: Lens.Lens' AssetPropertyVariant (Prelude.Maybe Prelude.Text)
assetPropertyVariant_doubleValue = Lens.lens (\AssetPropertyVariant' {doubleValue} -> doubleValue) (\s@AssetPropertyVariant' {} a -> s {doubleValue = a} :: AssetPropertyVariant)

-- | The asset property value is a Boolean value that must be @\'TRUE\'@ or
-- @\'FALSE\'@. You must use an expression, and the evaluated result should
-- be a Boolean value.
assetPropertyVariant_booleanValue :: Lens.Lens' AssetPropertyVariant (Prelude.Maybe Prelude.Text)
assetPropertyVariant_booleanValue = Lens.lens (\AssetPropertyVariant' {booleanValue} -> booleanValue) (\s@AssetPropertyVariant' {} a -> s {booleanValue = a} :: AssetPropertyVariant)

-- | The asset property value is a string. You must use an expression, and
-- the evaluated result should be a string.
assetPropertyVariant_stringValue :: Lens.Lens' AssetPropertyVariant (Prelude.Maybe Prelude.Text)
assetPropertyVariant_stringValue = Lens.lens (\AssetPropertyVariant' {stringValue} -> stringValue) (\s@AssetPropertyVariant' {} a -> s {stringValue = a} :: AssetPropertyVariant)

instance Data.FromJSON AssetPropertyVariant where
  parseJSON =
    Data.withObject
      "AssetPropertyVariant"
      ( \x ->
          AssetPropertyVariant'
            Prelude.<$> (x Data..:? "integerValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "booleanValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable AssetPropertyVariant where
  hashWithSalt _salt AssetPropertyVariant' {..} =
    _salt `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData AssetPropertyVariant where
  rnf AssetPropertyVariant' {..} =
    Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON AssetPropertyVariant where
  toJSON AssetPropertyVariant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("integerValue" Data..=) Prelude.<$> integerValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("booleanValue" Data..=) Prelude.<$> booleanValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )

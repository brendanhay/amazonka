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
-- Module      : Amazonka.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.NumberAttributeConstraintsType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum values of an attribute that is of the number
-- data type.
--
-- /See:/ 'newNumberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { -- | The maximum value of an attribute that is of the number data type.
    maxValue :: Prelude.Maybe Prelude.Text,
    -- | The minimum value of an attribute that is of the number data type.
    minValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberAttributeConstraintsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'numberAttributeConstraintsType_maxValue' - The maximum value of an attribute that is of the number data type.
--
-- 'minValue', 'numberAttributeConstraintsType_minValue' - The minimum value of an attribute that is of the number data type.
newNumberAttributeConstraintsType ::
  NumberAttributeConstraintsType
newNumberAttributeConstraintsType =
  NumberAttributeConstraintsType'
    { maxValue =
        Prelude.Nothing,
      minValue = Prelude.Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
numberAttributeConstraintsType_maxValue :: Lens.Lens' NumberAttributeConstraintsType (Prelude.Maybe Prelude.Text)
numberAttributeConstraintsType_maxValue = Lens.lens (\NumberAttributeConstraintsType' {maxValue} -> maxValue) (\s@NumberAttributeConstraintsType' {} a -> s {maxValue = a} :: NumberAttributeConstraintsType)

-- | The minimum value of an attribute that is of the number data type.
numberAttributeConstraintsType_minValue :: Lens.Lens' NumberAttributeConstraintsType (Prelude.Maybe Prelude.Text)
numberAttributeConstraintsType_minValue = Lens.lens (\NumberAttributeConstraintsType' {minValue} -> minValue) (\s@NumberAttributeConstraintsType' {} a -> s {minValue = a} :: NumberAttributeConstraintsType)

instance Data.FromJSON NumberAttributeConstraintsType where
  parseJSON =
    Data.withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            Prelude.<$> (x Data..:? "MaxValue")
            Prelude.<*> (x Data..:? "MinValue")
      )

instance
  Prelude.Hashable
    NumberAttributeConstraintsType
  where
  hashWithSalt
    _salt
    NumberAttributeConstraintsType' {..} =
      _salt
        `Prelude.hashWithSalt` maxValue
        `Prelude.hashWithSalt` minValue

instance
  Prelude.NFData
    NumberAttributeConstraintsType
  where
  rnf NumberAttributeConstraintsType' {..} =
    Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue

instance Data.ToJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxValue" Data..=) Prelude.<$> maxValue,
            ("MinValue" Data..=) Prelude.<$> minValue
          ]
      )

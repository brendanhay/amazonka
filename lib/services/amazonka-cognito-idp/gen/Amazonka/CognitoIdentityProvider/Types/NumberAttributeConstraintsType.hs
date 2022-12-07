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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The minimum value of an attribute that is of the number data type.
    minValue :: Prelude.Maybe Prelude.Text,
    -- | The maximum value of an attribute that is of the number data type.
    maxValue :: Prelude.Maybe Prelude.Text
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
-- 'minValue', 'numberAttributeConstraintsType_minValue' - The minimum value of an attribute that is of the number data type.
--
-- 'maxValue', 'numberAttributeConstraintsType_maxValue' - The maximum value of an attribute that is of the number data type.
newNumberAttributeConstraintsType ::
  NumberAttributeConstraintsType
newNumberAttributeConstraintsType =
  NumberAttributeConstraintsType'
    { minValue =
        Prelude.Nothing,
      maxValue = Prelude.Nothing
    }

-- | The minimum value of an attribute that is of the number data type.
numberAttributeConstraintsType_minValue :: Lens.Lens' NumberAttributeConstraintsType (Prelude.Maybe Prelude.Text)
numberAttributeConstraintsType_minValue = Lens.lens (\NumberAttributeConstraintsType' {minValue} -> minValue) (\s@NumberAttributeConstraintsType' {} a -> s {minValue = a} :: NumberAttributeConstraintsType)

-- | The maximum value of an attribute that is of the number data type.
numberAttributeConstraintsType_maxValue :: Lens.Lens' NumberAttributeConstraintsType (Prelude.Maybe Prelude.Text)
numberAttributeConstraintsType_maxValue = Lens.lens (\NumberAttributeConstraintsType' {maxValue} -> maxValue) (\s@NumberAttributeConstraintsType' {} a -> s {maxValue = a} :: NumberAttributeConstraintsType)

instance Data.FromJSON NumberAttributeConstraintsType where
  parseJSON =
    Data.withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            Prelude.<$> (x Data..:? "MinValue")
            Prelude.<*> (x Data..:? "MaxValue")
      )

instance
  Prelude.Hashable
    NumberAttributeConstraintsType
  where
  hashWithSalt
    _salt
    NumberAttributeConstraintsType' {..} =
      _salt `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    NumberAttributeConstraintsType
  where
  rnf NumberAttributeConstraintsType' {..} =
    Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf maxValue

instance Data.ToJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MinValue" Data..=) Prelude.<$> minValue,
            ("MaxValue" Data..=) Prelude.<$> maxValue
          ]
      )

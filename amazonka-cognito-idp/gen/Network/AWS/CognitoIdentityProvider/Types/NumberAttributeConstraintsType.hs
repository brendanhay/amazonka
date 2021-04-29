{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The minimum and maximum value of an attribute that is of the number data
-- type.
--
-- /See:/ 'newNumberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { -- | The maximum value of an attribute that is of the number data type.
    maxValue :: Prelude.Maybe Prelude.Text,
    -- | The minimum value of an attribute that is of the number data type.
    minValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    NumberAttributeConstraintsType
  where
  parseJSON =
    Prelude.withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            Prelude.<$> (x Prelude..:? "MaxValue")
            Prelude.<*> (x Prelude..:? "MinValue")
      )

instance
  Prelude.Hashable
    NumberAttributeConstraintsType

instance
  Prelude.NFData
    NumberAttributeConstraintsType

instance
  Prelude.ToJSON
    NumberAttributeConstraintsType
  where
  toJSON NumberAttributeConstraintsType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxValue" Prelude..=) Prelude.<$> maxValue,
            ("MinValue" Prelude..=) Prelude.<$> minValue
          ]
      )

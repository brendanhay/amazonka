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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The minimum and maximum value of an attribute that is of the number data
-- type.
--
-- /See:/ 'newNumberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { -- | The maximum value of an attribute that is of the number data type.
    maxValue :: Core.Maybe Core.Text,
    -- | The minimum value of an attribute that is of the number data type.
    minValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      minValue = Core.Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
numberAttributeConstraintsType_maxValue :: Lens.Lens' NumberAttributeConstraintsType (Core.Maybe Core.Text)
numberAttributeConstraintsType_maxValue = Lens.lens (\NumberAttributeConstraintsType' {maxValue} -> maxValue) (\s@NumberAttributeConstraintsType' {} a -> s {maxValue = a} :: NumberAttributeConstraintsType)

-- | The minimum value of an attribute that is of the number data type.
numberAttributeConstraintsType_minValue :: Lens.Lens' NumberAttributeConstraintsType (Core.Maybe Core.Text)
numberAttributeConstraintsType_minValue = Lens.lens (\NumberAttributeConstraintsType' {minValue} -> minValue) (\s@NumberAttributeConstraintsType' {} a -> s {minValue = a} :: NumberAttributeConstraintsType)

instance Core.FromJSON NumberAttributeConstraintsType where
  parseJSON =
    Core.withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            Core.<$> (x Core..:? "MaxValue")
            Core.<*> (x Core..:? "MinValue")
      )

instance Core.Hashable NumberAttributeConstraintsType

instance Core.NFData NumberAttributeConstraintsType

instance Core.ToJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxValue" Core..=) Core.<$> maxValue,
            ("MinValue" Core..=) Core.<$> minValue
          ]
      )

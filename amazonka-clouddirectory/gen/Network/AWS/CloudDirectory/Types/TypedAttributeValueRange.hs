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
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValueRange where

import Network.AWS.CloudDirectory.Types.RangeMode
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A range of attribute values. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_range_filters.html Range Filters>.
--
-- /See:/ 'newTypedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
  { -- | The attribute value to terminate the range at.
    endValue :: Prelude.Maybe TypedAttributeValue,
    -- | The value to start the range at.
    startValue :: Prelude.Maybe TypedAttributeValue,
    -- | The inclusive or exclusive range start.
    startMode :: RangeMode,
    -- | The inclusive or exclusive range end.
    endMode :: RangeMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TypedAttributeValueRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endValue', 'typedAttributeValueRange_endValue' - The attribute value to terminate the range at.
--
-- 'startValue', 'typedAttributeValueRange_startValue' - The value to start the range at.
--
-- 'startMode', 'typedAttributeValueRange_startMode' - The inclusive or exclusive range start.
--
-- 'endMode', 'typedAttributeValueRange_endMode' - The inclusive or exclusive range end.
newTypedAttributeValueRange ::
  -- | 'startMode'
  RangeMode ->
  -- | 'endMode'
  RangeMode ->
  TypedAttributeValueRange
newTypedAttributeValueRange pStartMode_ pEndMode_ =
  TypedAttributeValueRange'
    { endValue =
        Prelude.Nothing,
      startValue = Prelude.Nothing,
      startMode = pStartMode_,
      endMode = pEndMode_
    }

-- | The attribute value to terminate the range at.
typedAttributeValueRange_endValue :: Lens.Lens' TypedAttributeValueRange (Prelude.Maybe TypedAttributeValue)
typedAttributeValueRange_endValue = Lens.lens (\TypedAttributeValueRange' {endValue} -> endValue) (\s@TypedAttributeValueRange' {} a -> s {endValue = a} :: TypedAttributeValueRange)

-- | The value to start the range at.
typedAttributeValueRange_startValue :: Lens.Lens' TypedAttributeValueRange (Prelude.Maybe TypedAttributeValue)
typedAttributeValueRange_startValue = Lens.lens (\TypedAttributeValueRange' {startValue} -> startValue) (\s@TypedAttributeValueRange' {} a -> s {startValue = a} :: TypedAttributeValueRange)

-- | The inclusive or exclusive range start.
typedAttributeValueRange_startMode :: Lens.Lens' TypedAttributeValueRange RangeMode
typedAttributeValueRange_startMode = Lens.lens (\TypedAttributeValueRange' {startMode} -> startMode) (\s@TypedAttributeValueRange' {} a -> s {startMode = a} :: TypedAttributeValueRange)

-- | The inclusive or exclusive range end.
typedAttributeValueRange_endMode :: Lens.Lens' TypedAttributeValueRange RangeMode
typedAttributeValueRange_endMode = Lens.lens (\TypedAttributeValueRange' {endMode} -> endMode) (\s@TypedAttributeValueRange' {} a -> s {endMode = a} :: TypedAttributeValueRange)

instance Prelude.Hashable TypedAttributeValueRange

instance Prelude.NFData TypedAttributeValueRange

instance Prelude.ToJSON TypedAttributeValueRange where
  toJSON TypedAttributeValueRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndValue" Prelude..=) Prelude.<$> endValue,
            ("StartValue" Prelude..=) Prelude.<$> startValue,
            Prelude.Just ("StartMode" Prelude..= startMode),
            Prelude.Just ("EndMode" Prelude..= endMode)
          ]
      )

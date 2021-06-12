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
-- Module      : Network.AWS.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnnotationValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Value of a segment annotation. Has one of three value types: Number,
-- Boolean, or String.
--
-- /See:/ 'newAnnotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { -- | Value for a String annotation.
    stringValue :: Core.Maybe Core.Text,
    -- | Value for a Boolean annotation.
    booleanValue :: Core.Maybe Core.Bool,
    -- | Value for a Number annotation.
    numberValue :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnnotationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValue', 'annotationValue_stringValue' - Value for a String annotation.
--
-- 'booleanValue', 'annotationValue_booleanValue' - Value for a Boolean annotation.
--
-- 'numberValue', 'annotationValue_numberValue' - Value for a Number annotation.
newAnnotationValue ::
  AnnotationValue
newAnnotationValue =
  AnnotationValue'
    { stringValue = Core.Nothing,
      booleanValue = Core.Nothing,
      numberValue = Core.Nothing
    }

-- | Value for a String annotation.
annotationValue_stringValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Text)
annotationValue_stringValue = Lens.lens (\AnnotationValue' {stringValue} -> stringValue) (\s@AnnotationValue' {} a -> s {stringValue = a} :: AnnotationValue)

-- | Value for a Boolean annotation.
annotationValue_booleanValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Bool)
annotationValue_booleanValue = Lens.lens (\AnnotationValue' {booleanValue} -> booleanValue) (\s@AnnotationValue' {} a -> s {booleanValue = a} :: AnnotationValue)

-- | Value for a Number annotation.
annotationValue_numberValue :: Lens.Lens' AnnotationValue (Core.Maybe Core.Double)
annotationValue_numberValue = Lens.lens (\AnnotationValue' {numberValue} -> numberValue) (\s@AnnotationValue' {} a -> s {numberValue = a} :: AnnotationValue)

instance Core.FromJSON AnnotationValue where
  parseJSON =
    Core.withObject
      "AnnotationValue"
      ( \x ->
          AnnotationValue'
            Core.<$> (x Core..:? "StringValue")
            Core.<*> (x Core..:? "BooleanValue")
            Core.<*> (x Core..:? "NumberValue")
      )

instance Core.Hashable AnnotationValue

instance Core.NFData AnnotationValue

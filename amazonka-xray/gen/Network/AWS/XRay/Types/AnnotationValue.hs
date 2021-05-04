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
-- Module      : Network.AWS.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnnotationValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Value of a segment annotation. Has one of three value types: Number,
-- Boolean, or String.
--
-- /See:/ 'newAnnotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { -- | Value for a String annotation.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | Value for a Boolean annotation.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | Value for a Number annotation.
    numberValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stringValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing,
      numberValue = Prelude.Nothing
    }

-- | Value for a String annotation.
annotationValue_stringValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Text)
annotationValue_stringValue = Lens.lens (\AnnotationValue' {stringValue} -> stringValue) (\s@AnnotationValue' {} a -> s {stringValue = a} :: AnnotationValue)

-- | Value for a Boolean annotation.
annotationValue_booleanValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Bool)
annotationValue_booleanValue = Lens.lens (\AnnotationValue' {booleanValue} -> booleanValue) (\s@AnnotationValue' {} a -> s {booleanValue = a} :: AnnotationValue)

-- | Value for a Number annotation.
annotationValue_numberValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Double)
annotationValue_numberValue = Lens.lens (\AnnotationValue' {numberValue} -> numberValue) (\s@AnnotationValue' {} a -> s {numberValue = a} :: AnnotationValue)

instance Prelude.FromJSON AnnotationValue where
  parseJSON =
    Prelude.withObject
      "AnnotationValue"
      ( \x ->
          AnnotationValue'
            Prelude.<$> (x Prelude..:? "StringValue")
            Prelude.<*> (x Prelude..:? "BooleanValue")
            Prelude.<*> (x Prelude..:? "NumberValue")
      )

instance Prelude.Hashable AnnotationValue

instance Prelude.NFData AnnotationValue

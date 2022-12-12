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
-- Module      : Amazonka.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.AnnotationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Value of a segment annotation. Has one of three value types: Number,
-- Boolean, or String.
--
-- /See:/ 'newAnnotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { -- | Value for a Boolean annotation.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | Value for a Number annotation.
    numberValue :: Prelude.Maybe Prelude.Double,
    -- | Value for a String annotation.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnnotationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'booleanValue', 'annotationValue_booleanValue' - Value for a Boolean annotation.
--
-- 'numberValue', 'annotationValue_numberValue' - Value for a Number annotation.
--
-- 'stringValue', 'annotationValue_stringValue' - Value for a String annotation.
newAnnotationValue ::
  AnnotationValue
newAnnotationValue =
  AnnotationValue'
    { booleanValue = Prelude.Nothing,
      numberValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | Value for a Boolean annotation.
annotationValue_booleanValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Bool)
annotationValue_booleanValue = Lens.lens (\AnnotationValue' {booleanValue} -> booleanValue) (\s@AnnotationValue' {} a -> s {booleanValue = a} :: AnnotationValue)

-- | Value for a Number annotation.
annotationValue_numberValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Double)
annotationValue_numberValue = Lens.lens (\AnnotationValue' {numberValue} -> numberValue) (\s@AnnotationValue' {} a -> s {numberValue = a} :: AnnotationValue)

-- | Value for a String annotation.
annotationValue_stringValue :: Lens.Lens' AnnotationValue (Prelude.Maybe Prelude.Text)
annotationValue_stringValue = Lens.lens (\AnnotationValue' {stringValue} -> stringValue) (\s@AnnotationValue' {} a -> s {stringValue = a} :: AnnotationValue)

instance Data.FromJSON AnnotationValue where
  parseJSON =
    Data.withObject
      "AnnotationValue"
      ( \x ->
          AnnotationValue'
            Prelude.<$> (x Data..:? "BooleanValue")
            Prelude.<*> (x Data..:? "NumberValue")
            Prelude.<*> (x Data..:? "StringValue")
      )

instance Prelude.Hashable AnnotationValue where
  hashWithSalt _salt AnnotationValue' {..} =
    _salt `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` numberValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData AnnotationValue where
  rnf AnnotationValue' {..} =
    Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf numberValue
      `Prelude.seq` Prelude.rnf stringValue

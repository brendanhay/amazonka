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
-- Module      : Amazonka.Pinpoint.Types.AttributeDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.AttributeDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.AttributeType
import qualified Amazonka.Prelude as Prelude

-- | Specifies attribute-based criteria for including or excluding endpoints
-- from a segment.
--
-- /See:/ 'newAttributeDimension' smart constructor.
data AttributeDimension = AttributeDimension'
  { -- | The type of segment dimension to use. Valid values are:
    --
    -- INCLUSIVE - endpoints that have attributes matching the values are
    -- included in the segment.
    --
    -- EXCLUSIVE - endpoints that have attributes matching the values are
    -- excluded in the segment.
    --
    -- CONTAINS - endpoints that have attributes\' substrings match the values
    -- are included in the segment.
    --
    -- BEFORE - endpoints with attributes read as ISO_INSTANT datetimes before
    -- the value are included in the segment.
    --
    -- AFTER - endpoints with attributes read as ISO_INSTANT datetimes after
    -- the value are included in the segment.
    --
    -- ON - endpoints with attributes read as ISO_INSTANT dates on the value
    -- are included in the segment. Time is ignored in this comparison.
    --
    -- BETWEEN - endpoints with attributes read as ISO_INSTANT datetimes
    -- between the values are included in the segment.
    attributeType :: Prelude.Maybe AttributeType,
    -- | The criteria values to use for the segment dimension. Depending on the
    -- value of the AttributeType property, endpoints are included or excluded
    -- from the segment if their attribute values match the criteria values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeType', 'attributeDimension_attributeType' - The type of segment dimension to use. Valid values are:
--
-- INCLUSIVE - endpoints that have attributes matching the values are
-- included in the segment.
--
-- EXCLUSIVE - endpoints that have attributes matching the values are
-- excluded in the segment.
--
-- CONTAINS - endpoints that have attributes\' substrings match the values
-- are included in the segment.
--
-- BEFORE - endpoints with attributes read as ISO_INSTANT datetimes before
-- the value are included in the segment.
--
-- AFTER - endpoints with attributes read as ISO_INSTANT datetimes after
-- the value are included in the segment.
--
-- ON - endpoints with attributes read as ISO_INSTANT dates on the value
-- are included in the segment. Time is ignored in this comparison.
--
-- BETWEEN - endpoints with attributes read as ISO_INSTANT datetimes
-- between the values are included in the segment.
--
-- 'values', 'attributeDimension_values' - The criteria values to use for the segment dimension. Depending on the
-- value of the AttributeType property, endpoints are included or excluded
-- from the segment if their attribute values match the criteria values.
newAttributeDimension ::
  AttributeDimension
newAttributeDimension =
  AttributeDimension'
    { attributeType =
        Prelude.Nothing,
      values = Prelude.mempty
    }

-- | The type of segment dimension to use. Valid values are:
--
-- INCLUSIVE - endpoints that have attributes matching the values are
-- included in the segment.
--
-- EXCLUSIVE - endpoints that have attributes matching the values are
-- excluded in the segment.
--
-- CONTAINS - endpoints that have attributes\' substrings match the values
-- are included in the segment.
--
-- BEFORE - endpoints with attributes read as ISO_INSTANT datetimes before
-- the value are included in the segment.
--
-- AFTER - endpoints with attributes read as ISO_INSTANT datetimes after
-- the value are included in the segment.
--
-- ON - endpoints with attributes read as ISO_INSTANT dates on the value
-- are included in the segment. Time is ignored in this comparison.
--
-- BETWEEN - endpoints with attributes read as ISO_INSTANT datetimes
-- between the values are included in the segment.
attributeDimension_attributeType :: Lens.Lens' AttributeDimension (Prelude.Maybe AttributeType)
attributeDimension_attributeType = Lens.lens (\AttributeDimension' {attributeType} -> attributeType) (\s@AttributeDimension' {} a -> s {attributeType = a} :: AttributeDimension)

-- | The criteria values to use for the segment dimension. Depending on the
-- value of the AttributeType property, endpoints are included or excluded
-- from the segment if their attribute values match the criteria values.
attributeDimension_values :: Lens.Lens' AttributeDimension [Prelude.Text]
attributeDimension_values = Lens.lens (\AttributeDimension' {values} -> values) (\s@AttributeDimension' {} a -> s {values = a} :: AttributeDimension) Prelude.. Lens.coerced

instance Data.FromJSON AttributeDimension where
  parseJSON =
    Data.withObject
      "AttributeDimension"
      ( \x ->
          AttributeDimension'
            Prelude.<$> (x Data..:? "AttributeType")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AttributeDimension where
  hashWithSalt _salt AttributeDimension' {..} =
    _salt
      `Prelude.hashWithSalt` attributeType
      `Prelude.hashWithSalt` values

instance Prelude.NFData AttributeDimension where
  rnf AttributeDimension' {..} =
    Prelude.rnf attributeType
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON AttributeDimension where
  toJSON AttributeDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeType" Data..=) Prelude.<$> attributeType,
            Prelude.Just ("Values" Data..= values)
          ]
      )

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
-- Module      : Amazonka.SecurityHub.Types.MapFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.MapFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.MapFilterComparison

-- | A map filter for querying findings. Each map filter provides the field
-- to check, the value to look for, and the comparison operator.
--
-- /See:/ 'newMapFilter' smart constructor.
data MapFilter = MapFilter'
  { -- | The key of the map filter. For example, for @ResourceTags@, @Key@
    -- identifies the name of the tag. For @UserDefinedFields@, @Key@ is the
    -- name of the field.
    key :: Prelude.Maybe Prelude.Text,
    -- | The condition to apply to the key value when querying for findings with
    -- a map filter.
    --
    -- To search for values that exactly match the filter value, use @EQUALS@.
    -- For example, for the @ResourceTags@ field, the filter
    -- @Department EQUALS Security@ matches findings that have the value
    -- @Security@ for the tag @Department@.
    --
    -- To search for values other than the filter value, use @NOT_EQUALS@. For
    -- example, for the @ResourceTags@ field, the filter
    -- @Department NOT_EQUALS Finance@ matches findings that do not have the
    -- value @Finance@ for the tag @Department@.
    --
    -- @EQUALS@ filters on the same field are joined by @OR@. A finding matches
    -- if it matches any one of those filters.
    --
    -- @NOT_EQUALS@ filters on the same field are joined by @AND@. A finding
    -- matches only if it matches all of those filters.
    --
    -- You cannot have both an @EQUALS@ filter and a @NOT_EQUALS@ filter on the
    -- same field.
    comparison :: Prelude.Maybe MapFilterComparison,
    -- | The value for the key in the map filter. Filter values are case
    -- sensitive. For example, one of the values for a tag called @Department@
    -- might be @Security@. If you provide @security@ as the filter value, then
    -- there is no match.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'mapFilter_key' - The key of the map filter. For example, for @ResourceTags@, @Key@
-- identifies the name of the tag. For @UserDefinedFields@, @Key@ is the
-- name of the field.
--
-- 'comparison', 'mapFilter_comparison' - The condition to apply to the key value when querying for findings with
-- a map filter.
--
-- To search for values that exactly match the filter value, use @EQUALS@.
-- For example, for the @ResourceTags@ field, the filter
-- @Department EQUALS Security@ matches findings that have the value
-- @Security@ for the tag @Department@.
--
-- To search for values other than the filter value, use @NOT_EQUALS@. For
-- example, for the @ResourceTags@ field, the filter
-- @Department NOT_EQUALS Finance@ matches findings that do not have the
-- value @Finance@ for the tag @Department@.
--
-- @EQUALS@ filters on the same field are joined by @OR@. A finding matches
-- if it matches any one of those filters.
--
-- @NOT_EQUALS@ filters on the same field are joined by @AND@. A finding
-- matches only if it matches all of those filters.
--
-- You cannot have both an @EQUALS@ filter and a @NOT_EQUALS@ filter on the
-- same field.
--
-- 'value', 'mapFilter_value' - The value for the key in the map filter. Filter values are case
-- sensitive. For example, one of the values for a tag called @Department@
-- might be @Security@. If you provide @security@ as the filter value, then
-- there is no match.
newMapFilter ::
  MapFilter
newMapFilter =
  MapFilter'
    { key = Prelude.Nothing,
      comparison = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key of the map filter. For example, for @ResourceTags@, @Key@
-- identifies the name of the tag. For @UserDefinedFields@, @Key@ is the
-- name of the field.
mapFilter_key :: Lens.Lens' MapFilter (Prelude.Maybe Prelude.Text)
mapFilter_key = Lens.lens (\MapFilter' {key} -> key) (\s@MapFilter' {} a -> s {key = a} :: MapFilter)

-- | The condition to apply to the key value when querying for findings with
-- a map filter.
--
-- To search for values that exactly match the filter value, use @EQUALS@.
-- For example, for the @ResourceTags@ field, the filter
-- @Department EQUALS Security@ matches findings that have the value
-- @Security@ for the tag @Department@.
--
-- To search for values other than the filter value, use @NOT_EQUALS@. For
-- example, for the @ResourceTags@ field, the filter
-- @Department NOT_EQUALS Finance@ matches findings that do not have the
-- value @Finance@ for the tag @Department@.
--
-- @EQUALS@ filters on the same field are joined by @OR@. A finding matches
-- if it matches any one of those filters.
--
-- @NOT_EQUALS@ filters on the same field are joined by @AND@. A finding
-- matches only if it matches all of those filters.
--
-- You cannot have both an @EQUALS@ filter and a @NOT_EQUALS@ filter on the
-- same field.
mapFilter_comparison :: Lens.Lens' MapFilter (Prelude.Maybe MapFilterComparison)
mapFilter_comparison = Lens.lens (\MapFilter' {comparison} -> comparison) (\s@MapFilter' {} a -> s {comparison = a} :: MapFilter)

-- | The value for the key in the map filter. Filter values are case
-- sensitive. For example, one of the values for a tag called @Department@
-- might be @Security@. If you provide @security@ as the filter value, then
-- there is no match.
mapFilter_value :: Lens.Lens' MapFilter (Prelude.Maybe Prelude.Text)
mapFilter_value = Lens.lens (\MapFilter' {value} -> value) (\s@MapFilter' {} a -> s {value = a} :: MapFilter)

instance Core.FromJSON MapFilter where
  parseJSON =
    Core.withObject
      "MapFilter"
      ( \x ->
          MapFilter'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "Comparison")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable MapFilter where
  hashWithSalt _salt MapFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` value

instance Prelude.NFData MapFilter where
  rnf MapFilter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON MapFilter where
  toJSON MapFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("Comparison" Core..=) Prelude.<$> comparison,
            ("Value" Core..=) Prelude.<$> value
          ]
      )

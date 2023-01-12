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
-- Module      : Amazonka.SecurityHub.Types.StringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StringFilterComparison

-- | A string filter for querying findings.
--
-- /See:/ 'newStringFilter' smart constructor.
data StringFilter = StringFilter'
  { -- | The condition to apply to a string value when querying for findings. To
    -- search for values that contain the filter criteria value, use one of the
    -- following comparison operators:
    --
    -- -   To search for values that exactly match the filter value, use
    --     @EQUALS@.
    --
    --     For example, the filter @ResourceType EQUALS AwsEc2SecurityGroup@
    --     only matches findings that have a resource type of
    --     @AwsEc2SecurityGroup@.
    --
    -- -   To search for values that start with the filter value, use @PREFIX@.
    --
    --     For example, the filter @ResourceType PREFIX AwsIam@ matches
    --     findings that have a resource type that starts with @AwsIam@.
    --     Findings with a resource type of @AwsIamPolicy@, @AwsIamRole@, or
    --     @AwsIamUser@ would all match.
    --
    -- @EQUALS@ and @PREFIX@ filters on the same field are joined by @OR@. A
    -- finding matches if it matches any one of those filters.
    --
    -- To search for values that do not contain the filter criteria value, use
    -- one of the following comparison operators:
    --
    -- -   To search for values that do not exactly match the filter value, use
    --     @NOT_EQUALS@.
    --
    --     For example, the filter @ResourceType NOT_EQUALS AwsIamPolicy@
    --     matches findings that have a resource type other than
    --     @AwsIamPolicy@.
    --
    -- -   To search for values that do not start with the filter value, use
    --     @PREFIX_NOT_EQUALS@.
    --
    --     For example, the filter @ResourceType PREFIX_NOT_EQUALS AwsIam@
    --     matches findings that have a resource type that does not start with
    --     @AwsIam@. Findings with a resource type of @AwsIamPolicy@,
    --     @AwsIamRole@, or @AwsIamUser@ would all be excluded from the
    --     results.
    --
    -- @NOT_EQUALS@ and @PREFIX_NOT_EQUALS@ filters on the same field are
    -- joined by @AND@. A finding matches only if it matches all of those
    -- filters.
    --
    -- For filters on the same field, you cannot provide both an @EQUALS@
    -- filter and a @NOT_EQUALS@ or @PREFIX_NOT_EQUALS@ filter. Combining
    -- filters in this way always returns an error, even if the provided filter
    -- values would return valid results.
    --
    -- You can combine @PREFIX@ filters with @NOT_EQUALS@ or
    -- @PREFIX_NOT_EQUALS@ filters for the same field. Security Hub first
    -- processes the @PREFIX@ filters, then the @NOT_EQUALS@ or
    -- @PREFIX_NOT_EQUALS@ filters.
    --
    -- For example, for the following filter, Security Hub first identifies
    -- findings that have resource types that start with either @AwsIAM@ or
    -- @AwsEc2@. It then excludes findings that have a resource type of
    -- @AwsIamPolicy@ and findings that have a resource type of
    -- @AwsEc2NetworkInterface@.
    --
    -- -   @ResourceType PREFIX AwsIam@
    --
    -- -   @ResourceType PREFIX AwsEc2@
    --
    -- -   @ResourceType NOT_EQUALS AwsIamPolicy@
    --
    -- -   @ResourceType NOT_EQUALS AwsEc2NetworkInterface@
    comparison :: Prelude.Maybe StringFilterComparison,
    -- | The string filter value. Filter values are case sensitive. For example,
    -- the product name for control-based findings is @Security Hub@. If you
    -- provide @security hub@ as the filter text, then there is no match.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'stringFilter_comparison' - The condition to apply to a string value when querying for findings. To
-- search for values that contain the filter criteria value, use one of the
-- following comparison operators:
--
-- -   To search for values that exactly match the filter value, use
--     @EQUALS@.
--
--     For example, the filter @ResourceType EQUALS AwsEc2SecurityGroup@
--     only matches findings that have a resource type of
--     @AwsEc2SecurityGroup@.
--
-- -   To search for values that start with the filter value, use @PREFIX@.
--
--     For example, the filter @ResourceType PREFIX AwsIam@ matches
--     findings that have a resource type that starts with @AwsIam@.
--     Findings with a resource type of @AwsIamPolicy@, @AwsIamRole@, or
--     @AwsIamUser@ would all match.
--
-- @EQUALS@ and @PREFIX@ filters on the same field are joined by @OR@. A
-- finding matches if it matches any one of those filters.
--
-- To search for values that do not contain the filter criteria value, use
-- one of the following comparison operators:
--
-- -   To search for values that do not exactly match the filter value, use
--     @NOT_EQUALS@.
--
--     For example, the filter @ResourceType NOT_EQUALS AwsIamPolicy@
--     matches findings that have a resource type other than
--     @AwsIamPolicy@.
--
-- -   To search for values that do not start with the filter value, use
--     @PREFIX_NOT_EQUALS@.
--
--     For example, the filter @ResourceType PREFIX_NOT_EQUALS AwsIam@
--     matches findings that have a resource type that does not start with
--     @AwsIam@. Findings with a resource type of @AwsIamPolicy@,
--     @AwsIamRole@, or @AwsIamUser@ would all be excluded from the
--     results.
--
-- @NOT_EQUALS@ and @PREFIX_NOT_EQUALS@ filters on the same field are
-- joined by @AND@. A finding matches only if it matches all of those
-- filters.
--
-- For filters on the same field, you cannot provide both an @EQUALS@
-- filter and a @NOT_EQUALS@ or @PREFIX_NOT_EQUALS@ filter. Combining
-- filters in this way always returns an error, even if the provided filter
-- values would return valid results.
--
-- You can combine @PREFIX@ filters with @NOT_EQUALS@ or
-- @PREFIX_NOT_EQUALS@ filters for the same field. Security Hub first
-- processes the @PREFIX@ filters, then the @NOT_EQUALS@ or
-- @PREFIX_NOT_EQUALS@ filters.
--
-- For example, for the following filter, Security Hub first identifies
-- findings that have resource types that start with either @AwsIAM@ or
-- @AwsEc2@. It then excludes findings that have a resource type of
-- @AwsIamPolicy@ and findings that have a resource type of
-- @AwsEc2NetworkInterface@.
--
-- -   @ResourceType PREFIX AwsIam@
--
-- -   @ResourceType PREFIX AwsEc2@
--
-- -   @ResourceType NOT_EQUALS AwsIamPolicy@
--
-- -   @ResourceType NOT_EQUALS AwsEc2NetworkInterface@
--
-- 'value', 'stringFilter_value' - The string filter value. Filter values are case sensitive. For example,
-- the product name for control-based findings is @Security Hub@. If you
-- provide @security hub@ as the filter text, then there is no match.
newStringFilter ::
  StringFilter
newStringFilter =
  StringFilter'
    { comparison = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The condition to apply to a string value when querying for findings. To
-- search for values that contain the filter criteria value, use one of the
-- following comparison operators:
--
-- -   To search for values that exactly match the filter value, use
--     @EQUALS@.
--
--     For example, the filter @ResourceType EQUALS AwsEc2SecurityGroup@
--     only matches findings that have a resource type of
--     @AwsEc2SecurityGroup@.
--
-- -   To search for values that start with the filter value, use @PREFIX@.
--
--     For example, the filter @ResourceType PREFIX AwsIam@ matches
--     findings that have a resource type that starts with @AwsIam@.
--     Findings with a resource type of @AwsIamPolicy@, @AwsIamRole@, or
--     @AwsIamUser@ would all match.
--
-- @EQUALS@ and @PREFIX@ filters on the same field are joined by @OR@. A
-- finding matches if it matches any one of those filters.
--
-- To search for values that do not contain the filter criteria value, use
-- one of the following comparison operators:
--
-- -   To search for values that do not exactly match the filter value, use
--     @NOT_EQUALS@.
--
--     For example, the filter @ResourceType NOT_EQUALS AwsIamPolicy@
--     matches findings that have a resource type other than
--     @AwsIamPolicy@.
--
-- -   To search for values that do not start with the filter value, use
--     @PREFIX_NOT_EQUALS@.
--
--     For example, the filter @ResourceType PREFIX_NOT_EQUALS AwsIam@
--     matches findings that have a resource type that does not start with
--     @AwsIam@. Findings with a resource type of @AwsIamPolicy@,
--     @AwsIamRole@, or @AwsIamUser@ would all be excluded from the
--     results.
--
-- @NOT_EQUALS@ and @PREFIX_NOT_EQUALS@ filters on the same field are
-- joined by @AND@. A finding matches only if it matches all of those
-- filters.
--
-- For filters on the same field, you cannot provide both an @EQUALS@
-- filter and a @NOT_EQUALS@ or @PREFIX_NOT_EQUALS@ filter. Combining
-- filters in this way always returns an error, even if the provided filter
-- values would return valid results.
--
-- You can combine @PREFIX@ filters with @NOT_EQUALS@ or
-- @PREFIX_NOT_EQUALS@ filters for the same field. Security Hub first
-- processes the @PREFIX@ filters, then the @NOT_EQUALS@ or
-- @PREFIX_NOT_EQUALS@ filters.
--
-- For example, for the following filter, Security Hub first identifies
-- findings that have resource types that start with either @AwsIAM@ or
-- @AwsEc2@. It then excludes findings that have a resource type of
-- @AwsIamPolicy@ and findings that have a resource type of
-- @AwsEc2NetworkInterface@.
--
-- -   @ResourceType PREFIX AwsIam@
--
-- -   @ResourceType PREFIX AwsEc2@
--
-- -   @ResourceType NOT_EQUALS AwsIamPolicy@
--
-- -   @ResourceType NOT_EQUALS AwsEc2NetworkInterface@
stringFilter_comparison :: Lens.Lens' StringFilter (Prelude.Maybe StringFilterComparison)
stringFilter_comparison = Lens.lens (\StringFilter' {comparison} -> comparison) (\s@StringFilter' {} a -> s {comparison = a} :: StringFilter)

-- | The string filter value. Filter values are case sensitive. For example,
-- the product name for control-based findings is @Security Hub@. If you
-- provide @security hub@ as the filter text, then there is no match.
stringFilter_value :: Lens.Lens' StringFilter (Prelude.Maybe Prelude.Text)
stringFilter_value = Lens.lens (\StringFilter' {value} -> value) (\s@StringFilter' {} a -> s {value = a} :: StringFilter)

instance Data.FromJSON StringFilter where
  parseJSON =
    Data.withObject
      "StringFilter"
      ( \x ->
          StringFilter'
            Prelude.<$> (x Data..:? "Comparison")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable StringFilter where
  hashWithSalt _salt StringFilter' {..} =
    _salt `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` value

instance Prelude.NFData StringFilter where
  rnf StringFilter' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON StringFilter where
  toJSON StringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comparison" Data..=) Prelude.<$> comparison,
            ("Value" Data..=) Prelude.<$> value
          ]
      )

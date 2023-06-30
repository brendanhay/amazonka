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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRulePredicateListDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRulePredicateListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the @ByteMatchSet@, @IPSet@,
-- @SqlInjectionMatchSet@, @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@,
-- and @SizeConstraintSet@ objects that you want to add to a rule and, for
-- each object, indicates whether you want to negate the settings.
--
-- /See:/ 'newAwsWafRegionalRulePredicateListDetails' smart constructor.
data AwsWafRegionalRulePredicateListDetails = AwsWafRegionalRulePredicateListDetails'
  { -- | A unique identifier for a predicate in a rule, such as @ByteMatchSetId@
    -- or @IPSetId@.
    dataId :: Prelude.Maybe Prelude.Text,
    -- | Specifies if you want WAF to allow, block, or count requests based on
    -- the settings in the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
    -- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, or @SizeConstraintSet@.
    negated :: Prelude.Maybe Prelude.Bool,
    -- | The type of predicate in a rule, such as @ByteMatch@ or @IPSet@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRulePredicateListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataId', 'awsWafRegionalRulePredicateListDetails_dataId' - A unique identifier for a predicate in a rule, such as @ByteMatchSetId@
-- or @IPSetId@.
--
-- 'negated', 'awsWafRegionalRulePredicateListDetails_negated' - Specifies if you want WAF to allow, block, or count requests based on
-- the settings in the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, or @SizeConstraintSet@.
--
-- 'type'', 'awsWafRegionalRulePredicateListDetails_type' - The type of predicate in a rule, such as @ByteMatch@ or @IPSet@.
newAwsWafRegionalRulePredicateListDetails ::
  AwsWafRegionalRulePredicateListDetails
newAwsWafRegionalRulePredicateListDetails =
  AwsWafRegionalRulePredicateListDetails'
    { dataId =
        Prelude.Nothing,
      negated = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for a predicate in a rule, such as @ByteMatchSetId@
-- or @IPSetId@.
awsWafRegionalRulePredicateListDetails_dataId :: Lens.Lens' AwsWafRegionalRulePredicateListDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRulePredicateListDetails_dataId = Lens.lens (\AwsWafRegionalRulePredicateListDetails' {dataId} -> dataId) (\s@AwsWafRegionalRulePredicateListDetails' {} a -> s {dataId = a} :: AwsWafRegionalRulePredicateListDetails)

-- | Specifies if you want WAF to allow, block, or count requests based on
-- the settings in the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, or @SizeConstraintSet@.
awsWafRegionalRulePredicateListDetails_negated :: Lens.Lens' AwsWafRegionalRulePredicateListDetails (Prelude.Maybe Prelude.Bool)
awsWafRegionalRulePredicateListDetails_negated = Lens.lens (\AwsWafRegionalRulePredicateListDetails' {negated} -> negated) (\s@AwsWafRegionalRulePredicateListDetails' {} a -> s {negated = a} :: AwsWafRegionalRulePredicateListDetails)

-- | The type of predicate in a rule, such as @ByteMatch@ or @IPSet@.
awsWafRegionalRulePredicateListDetails_type :: Lens.Lens' AwsWafRegionalRulePredicateListDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalRulePredicateListDetails_type = Lens.lens (\AwsWafRegionalRulePredicateListDetails' {type'} -> type') (\s@AwsWafRegionalRulePredicateListDetails' {} a -> s {type' = a} :: AwsWafRegionalRulePredicateListDetails)

instance
  Data.FromJSON
    AwsWafRegionalRulePredicateListDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalRulePredicateListDetails"
      ( \x ->
          AwsWafRegionalRulePredicateListDetails'
            Prelude.<$> (x Data..:? "DataId")
            Prelude.<*> (x Data..:? "Negated")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRulePredicateListDetails
  where
  hashWithSalt
    _salt
    AwsWafRegionalRulePredicateListDetails' {..} =
      _salt
        `Prelude.hashWithSalt` dataId
        `Prelude.hashWithSalt` negated
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalRulePredicateListDetails
  where
  rnf AwsWafRegionalRulePredicateListDetails' {..} =
    Prelude.rnf dataId
      `Prelude.seq` Prelude.rnf negated
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRegionalRulePredicateListDetails
  where
  toJSON AwsWafRegionalRulePredicateListDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataId" Data..=) Prelude.<$> dataId,
            ("Negated" Data..=) Prelude.<$> negated,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

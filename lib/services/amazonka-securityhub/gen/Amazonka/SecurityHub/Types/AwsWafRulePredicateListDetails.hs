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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRulePredicateListDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRulePredicateListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the @ByteMatchSet@, @IPSet@,
-- @SqlInjectionMatchSet@, @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@,
-- and @SizeConstraintSet@ objects that you want to add to a rule and, for
-- each object, indicates whether you want to negate the settings.
--
-- /See:/ 'newAwsWafRulePredicateListDetails' smart constructor.
data AwsWafRulePredicateListDetails = AwsWafRulePredicateListDetails'
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
-- Create a value of 'AwsWafRulePredicateListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataId', 'awsWafRulePredicateListDetails_dataId' - A unique identifier for a predicate in a rule, such as @ByteMatchSetId@
-- or @IPSetId@.
--
-- 'negated', 'awsWafRulePredicateListDetails_negated' - Specifies if you want WAF to allow, block, or count requests based on
-- the settings in the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, or @SizeConstraintSet@.
--
-- 'type'', 'awsWafRulePredicateListDetails_type' - The type of predicate in a rule, such as @ByteMatch@ or @IPSet@.
newAwsWafRulePredicateListDetails ::
  AwsWafRulePredicateListDetails
newAwsWafRulePredicateListDetails =
  AwsWafRulePredicateListDetails'
    { dataId =
        Prelude.Nothing,
      negated = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for a predicate in a rule, such as @ByteMatchSetId@
-- or @IPSetId@.
awsWafRulePredicateListDetails_dataId :: Lens.Lens' AwsWafRulePredicateListDetails (Prelude.Maybe Prelude.Text)
awsWafRulePredicateListDetails_dataId = Lens.lens (\AwsWafRulePredicateListDetails' {dataId} -> dataId) (\s@AwsWafRulePredicateListDetails' {} a -> s {dataId = a} :: AwsWafRulePredicateListDetails)

-- | Specifies if you want WAF to allow, block, or count requests based on
-- the settings in the @ByteMatchSet@, @IPSet@, @SqlInjectionMatchSet@,
-- @XssMatchSet@, @RegexMatchSet@, @GeoMatchSet@, or @SizeConstraintSet@.
awsWafRulePredicateListDetails_negated :: Lens.Lens' AwsWafRulePredicateListDetails (Prelude.Maybe Prelude.Bool)
awsWafRulePredicateListDetails_negated = Lens.lens (\AwsWafRulePredicateListDetails' {negated} -> negated) (\s@AwsWafRulePredicateListDetails' {} a -> s {negated = a} :: AwsWafRulePredicateListDetails)

-- | The type of predicate in a rule, such as @ByteMatch@ or @IPSet@.
awsWafRulePredicateListDetails_type :: Lens.Lens' AwsWafRulePredicateListDetails (Prelude.Maybe Prelude.Text)
awsWafRulePredicateListDetails_type = Lens.lens (\AwsWafRulePredicateListDetails' {type'} -> type') (\s@AwsWafRulePredicateListDetails' {} a -> s {type' = a} :: AwsWafRulePredicateListDetails)

instance Data.FromJSON AwsWafRulePredicateListDetails where
  parseJSON =
    Data.withObject
      "AwsWafRulePredicateListDetails"
      ( \x ->
          AwsWafRulePredicateListDetails'
            Prelude.<$> (x Data..:? "DataId")
            Prelude.<*> (x Data..:? "Negated")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRulePredicateListDetails
  where
  hashWithSalt
    _salt
    AwsWafRulePredicateListDetails' {..} =
      _salt `Prelude.hashWithSalt` dataId
        `Prelude.hashWithSalt` negated
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRulePredicateListDetails
  where
  rnf AwsWafRulePredicateListDetails' {..} =
    Prelude.rnf dataId
      `Prelude.seq` Prelude.rnf negated
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AwsWafRulePredicateListDetails where
  toJSON AwsWafRulePredicateListDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataId" Data..=) Prelude.<$> dataId,
            ("Negated" Data..=) Prelude.<$> negated,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

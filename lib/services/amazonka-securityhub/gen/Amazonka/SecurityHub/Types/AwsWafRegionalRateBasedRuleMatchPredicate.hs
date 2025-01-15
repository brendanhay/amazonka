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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for a match predicate. A predicate might look for
-- characteristics such as specific IP addresses, geographic locations, or
-- sizes.
--
-- /See:/ 'newAwsWafRegionalRateBasedRuleMatchPredicate' smart constructor.
data AwsWafRegionalRateBasedRuleMatchPredicate = AwsWafRegionalRateBasedRuleMatchPredicate'
  { -- | The unique identifier for the predicate.
    dataId :: Prelude.Maybe Prelude.Text,
    -- | If set to @true@, then the rule actions are performed on requests that
    -- match the predicate settings.
    --
    -- If set to @false@, then the rule actions are performed on all requests
    -- except those that match the predicate settings.
    negated :: Prelude.Maybe Prelude.Bool,
    -- | The type of predicate. Valid values are as follows:
    --
    -- -   @ByteMatch@
    --
    -- -   @GeoMatch@
    --
    -- -   @IPMatch@
    --
    -- -   @RegexMatch@
    --
    -- -   @SizeConstraint@
    --
    -- -   @SqlInjectionMatch@
    --
    -- -   @XssMatch@
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalRateBasedRuleMatchPredicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataId', 'awsWafRegionalRateBasedRuleMatchPredicate_dataId' - The unique identifier for the predicate.
--
-- 'negated', 'awsWafRegionalRateBasedRuleMatchPredicate_negated' - If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
--
-- 'type'', 'awsWafRegionalRateBasedRuleMatchPredicate_type' - The type of predicate. Valid values are as follows:
--
-- -   @ByteMatch@
--
-- -   @GeoMatch@
--
-- -   @IPMatch@
--
-- -   @RegexMatch@
--
-- -   @SizeConstraint@
--
-- -   @SqlInjectionMatch@
--
-- -   @XssMatch@
newAwsWafRegionalRateBasedRuleMatchPredicate ::
  AwsWafRegionalRateBasedRuleMatchPredicate
newAwsWafRegionalRateBasedRuleMatchPredicate =
  AwsWafRegionalRateBasedRuleMatchPredicate'
    { dataId =
        Prelude.Nothing,
      negated = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier for the predicate.
awsWafRegionalRateBasedRuleMatchPredicate_dataId :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleMatchPredicate_dataId = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {dataId} -> dataId) (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {dataId = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

-- | If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
awsWafRegionalRateBasedRuleMatchPredicate_negated :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Bool)
awsWafRegionalRateBasedRuleMatchPredicate_negated = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {negated} -> negated) (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {negated = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

-- | The type of predicate. Valid values are as follows:
--
-- -   @ByteMatch@
--
-- -   @GeoMatch@
--
-- -   @IPMatch@
--
-- -   @RegexMatch@
--
-- -   @SizeConstraint@
--
-- -   @SqlInjectionMatch@
--
-- -   @XssMatch@
awsWafRegionalRateBasedRuleMatchPredicate_type :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleMatchPredicate_type = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {type'} -> type') (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {type' = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

instance
  Data.FromJSON
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  parseJSON =
    Data.withObject
      "AwsWafRegionalRateBasedRuleMatchPredicate"
      ( \x ->
          AwsWafRegionalRateBasedRuleMatchPredicate'
            Prelude.<$> (x Data..:? "DataId")
            Prelude.<*> (x Data..:? "Negated")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  hashWithSalt
    _salt
    AwsWafRegionalRateBasedRuleMatchPredicate' {..} =
      _salt
        `Prelude.hashWithSalt` dataId
        `Prelude.hashWithSalt` negated
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  rnf AwsWafRegionalRateBasedRuleMatchPredicate' {..} =
    Prelude.rnf dataId `Prelude.seq`
      Prelude.rnf negated `Prelude.seq`
        Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  toJSON AwsWafRegionalRateBasedRuleMatchPredicate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataId" Data..=) Prelude.<$> dataId,
            ("Negated" Data..=) Prelude.<$> negated,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

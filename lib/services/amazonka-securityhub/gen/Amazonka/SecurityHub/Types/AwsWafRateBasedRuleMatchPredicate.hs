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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A match predicate. A predicate might look for characteristics such as
-- specific IP addresses, geographic locations, or sizes.
--
-- /See:/ 'newAwsWafRateBasedRuleMatchPredicate' smart constructor.
data AwsWafRateBasedRuleMatchPredicate = AwsWafRateBasedRuleMatchPredicate'
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
-- Create a value of 'AwsWafRateBasedRuleMatchPredicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataId', 'awsWafRateBasedRuleMatchPredicate_dataId' - The unique identifier for the predicate.
--
-- 'negated', 'awsWafRateBasedRuleMatchPredicate_negated' - If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
--
-- 'type'', 'awsWafRateBasedRuleMatchPredicate_type' - The type of predicate. Valid values are as follows:
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
newAwsWafRateBasedRuleMatchPredicate ::
  AwsWafRateBasedRuleMatchPredicate
newAwsWafRateBasedRuleMatchPredicate =
  AwsWafRateBasedRuleMatchPredicate'
    { dataId =
        Prelude.Nothing,
      negated = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier for the predicate.
awsWafRateBasedRuleMatchPredicate_dataId :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleMatchPredicate_dataId = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {dataId} -> dataId) (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {dataId = a} :: AwsWafRateBasedRuleMatchPredicate)

-- | If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
awsWafRateBasedRuleMatchPredicate_negated :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Bool)
awsWafRateBasedRuleMatchPredicate_negated = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {negated} -> negated) (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {negated = a} :: AwsWafRateBasedRuleMatchPredicate)

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
awsWafRateBasedRuleMatchPredicate_type :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleMatchPredicate_type = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {type'} -> type') (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {type' = a} :: AwsWafRateBasedRuleMatchPredicate)

instance
  Data.FromJSON
    AwsWafRateBasedRuleMatchPredicate
  where
  parseJSON =
    Data.withObject
      "AwsWafRateBasedRuleMatchPredicate"
      ( \x ->
          AwsWafRateBasedRuleMatchPredicate'
            Prelude.<$> (x Data..:? "DataId")
            Prelude.<*> (x Data..:? "Negated")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRateBasedRuleMatchPredicate
  where
  hashWithSalt
    _salt
    AwsWafRateBasedRuleMatchPredicate' {..} =
      _salt `Prelude.hashWithSalt` dataId
        `Prelude.hashWithSalt` negated
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsWafRateBasedRuleMatchPredicate
  where
  rnf AwsWafRateBasedRuleMatchPredicate' {..} =
    Prelude.rnf dataId
      `Prelude.seq` Prelude.rnf negated
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsWafRateBasedRuleMatchPredicate
  where
  toJSON AwsWafRateBasedRuleMatchPredicate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataId" Data..=) Prelude.<$> dataId,
            ("Negated" Data..=) Prelude.<$> negated,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

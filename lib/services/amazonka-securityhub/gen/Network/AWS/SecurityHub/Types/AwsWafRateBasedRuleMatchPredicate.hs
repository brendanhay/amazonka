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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A match predicate. A predicate might look for characteristics such as
-- specific IP addresses, geographic locations, or sizes.
--
-- /See:/ 'newAwsWafRateBasedRuleMatchPredicate' smart constructor.
data AwsWafRateBasedRuleMatchPredicate = AwsWafRateBasedRuleMatchPredicate'
  { -- | If set to @true@, then the rule actions are performed on requests that
    -- match the predicate settings.
    --
    -- If set to @false@, then the rule actions are performed on all requests
    -- except those that match the predicate settings.
    negated :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the predicate.
    dataId :: Prelude.Maybe Prelude.Text,
    -- | The type of predicate.
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
-- 'negated', 'awsWafRateBasedRuleMatchPredicate_negated' - If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
--
-- 'dataId', 'awsWafRateBasedRuleMatchPredicate_dataId' - The unique identifier for the predicate.
--
-- 'type'', 'awsWafRateBasedRuleMatchPredicate_type' - The type of predicate.
newAwsWafRateBasedRuleMatchPredicate ::
  AwsWafRateBasedRuleMatchPredicate
newAwsWafRateBasedRuleMatchPredicate =
  AwsWafRateBasedRuleMatchPredicate'
    { negated =
        Prelude.Nothing,
      dataId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
awsWafRateBasedRuleMatchPredicate_negated :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Bool)
awsWafRateBasedRuleMatchPredicate_negated = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {negated} -> negated) (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {negated = a} :: AwsWafRateBasedRuleMatchPredicate)

-- | The unique identifier for the predicate.
awsWafRateBasedRuleMatchPredicate_dataId :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleMatchPredicate_dataId = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {dataId} -> dataId) (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {dataId = a} :: AwsWafRateBasedRuleMatchPredicate)

-- | The type of predicate.
awsWafRateBasedRuleMatchPredicate_type :: Lens.Lens' AwsWafRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRateBasedRuleMatchPredicate_type = Lens.lens (\AwsWafRateBasedRuleMatchPredicate' {type'} -> type') (\s@AwsWafRateBasedRuleMatchPredicate' {} a -> s {type' = a} :: AwsWafRateBasedRuleMatchPredicate)

instance
  Core.FromJSON
    AwsWafRateBasedRuleMatchPredicate
  where
  parseJSON =
    Core.withObject
      "AwsWafRateBasedRuleMatchPredicate"
      ( \x ->
          AwsWafRateBasedRuleMatchPredicate'
            Prelude.<$> (x Core..:? "Negated")
            Prelude.<*> (x Core..:? "DataId")
            Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRateBasedRuleMatchPredicate

instance
  Prelude.NFData
    AwsWafRateBasedRuleMatchPredicate

instance
  Core.ToJSON
    AwsWafRateBasedRuleMatchPredicate
  where
  toJSON AwsWafRateBasedRuleMatchPredicate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Negated" Core..=) Prelude.<$> negated,
            ("DataId" Core..=) Prelude.<$> dataId,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )

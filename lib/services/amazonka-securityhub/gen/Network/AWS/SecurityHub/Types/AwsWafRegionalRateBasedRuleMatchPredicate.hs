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
-- Module      : Network.AWS.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details for a match predicate. A predicate might look for
-- characteristics such as specific IP addresses, geographic locations, or
-- sizes.
--
-- /See:/ 'newAwsWafRegionalRateBasedRuleMatchPredicate' smart constructor.
data AwsWafRegionalRateBasedRuleMatchPredicate = AwsWafRegionalRateBasedRuleMatchPredicate'
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
-- Create a value of 'AwsWafRegionalRateBasedRuleMatchPredicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negated', 'awsWafRegionalRateBasedRuleMatchPredicate_negated' - If set to @true@, then the rule actions are performed on requests that
-- match the predicate settings.
--
-- If set to @false@, then the rule actions are performed on all requests
-- except those that match the predicate settings.
--
-- 'dataId', 'awsWafRegionalRateBasedRuleMatchPredicate_dataId' - The unique identifier for the predicate.
--
-- 'type'', 'awsWafRegionalRateBasedRuleMatchPredicate_type' - The type of predicate.
newAwsWafRegionalRateBasedRuleMatchPredicate ::
  AwsWafRegionalRateBasedRuleMatchPredicate
newAwsWafRegionalRateBasedRuleMatchPredicate =
  AwsWafRegionalRateBasedRuleMatchPredicate'
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
awsWafRegionalRateBasedRuleMatchPredicate_negated :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Bool)
awsWafRegionalRateBasedRuleMatchPredicate_negated = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {negated} -> negated) (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {negated = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

-- | The unique identifier for the predicate.
awsWafRegionalRateBasedRuleMatchPredicate_dataId :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleMatchPredicate_dataId = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {dataId} -> dataId) (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {dataId = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

-- | The type of predicate.
awsWafRegionalRateBasedRuleMatchPredicate_type :: Lens.Lens' AwsWafRegionalRateBasedRuleMatchPredicate (Prelude.Maybe Prelude.Text)
awsWafRegionalRateBasedRuleMatchPredicate_type = Lens.lens (\AwsWafRegionalRateBasedRuleMatchPredicate' {type'} -> type') (\s@AwsWafRegionalRateBasedRuleMatchPredicate' {} a -> s {type' = a} :: AwsWafRegionalRateBasedRuleMatchPredicate)

instance
  Core.FromJSON
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  parseJSON =
    Core.withObject
      "AwsWafRegionalRateBasedRuleMatchPredicate"
      ( \x ->
          AwsWafRegionalRateBasedRuleMatchPredicate'
            Prelude.<$> (x Core..:? "Negated")
              Prelude.<*> (x Core..:? "DataId")
              Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsWafRegionalRateBasedRuleMatchPredicate

instance
  Prelude.NFData
    AwsWafRegionalRateBasedRuleMatchPredicate

instance
  Core.ToJSON
    AwsWafRegionalRateBasedRuleMatchPredicate
  where
  toJSON AwsWafRegionalRateBasedRuleMatchPredicate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Negated" Core..=) Prelude.<$> negated,
            ("DataId" Core..=) Prelude.<$> dataId,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )

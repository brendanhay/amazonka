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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A destination IP address or range.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleMatchAttributesDestinations' smart constructor.
data RuleGroupSourceStatelessRuleMatchAttributesDestinations = RuleGroupSourceStatelessRuleMatchAttributesDestinations'
  { -- | An IP address or a block of IP addresses.
    addressDefinition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleMatchAttributesDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressDefinition', 'ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition' - An IP address or a block of IP addresses.
newRuleGroupSourceStatelessRuleMatchAttributesDestinations ::
  RuleGroupSourceStatelessRuleMatchAttributesDestinations
newRuleGroupSourceStatelessRuleMatchAttributesDestinations =
  RuleGroupSourceStatelessRuleMatchAttributesDestinations'
    { addressDefinition =
        Prelude.Nothing
    }

-- | An IP address or a block of IP addresses.
ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesDestinations (Prelude.Maybe Prelude.Text)
ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesDestinations' {addressDefinition} -> addressDefinition) (\s@RuleGroupSourceStatelessRuleMatchAttributesDestinations' {} a -> s {addressDefinition = a} :: RuleGroupSourceStatelessRuleMatchAttributesDestinations)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinations
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRuleMatchAttributesDestinations"
      ( \x ->
          RuleGroupSourceStatelessRuleMatchAttributesDestinations'
            Prelude.<$> (x Data..:? "AddressDefinition")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleMatchAttributesDestinations
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleMatchAttributesDestinations' {..} =
      _salt `Prelude.hashWithSalt` addressDefinition

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleMatchAttributesDestinations
  where
  rnf
    RuleGroupSourceStatelessRuleMatchAttributesDestinations' {..} =
      Prelude.rnf addressDefinition

instance
  Data.ToJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinations
  where
  toJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinations' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AddressDefinition" Data..=)
                Prelude.<$> addressDefinition
            ]
        )

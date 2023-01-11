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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesTcpFlags where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of TCP flags and masks to inspect for.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleMatchAttributesTcpFlags' smart constructor.
data RuleGroupSourceStatelessRuleMatchAttributesTcpFlags = RuleGroupSourceStatelessRuleMatchAttributesTcpFlags'
  { -- | Defines the flags from the @Masks@ setting that must be set in order for
    -- the packet to match. Flags that are listed must be set. Flags that are
    -- not listed must not be set.
    flags :: Prelude.Maybe [Prelude.Text],
    -- | The set of flags to consider in the inspection. If not specified, then
    -- all flags are inspected.
    masks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flags', 'ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags' - Defines the flags from the @Masks@ setting that must be set in order for
-- the packet to match. Flags that are listed must be set. Flags that are
-- not listed must not be set.
--
-- 'masks', 'ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks' - The set of flags to consider in the inspection. If not specified, then
-- all flags are inspected.
newRuleGroupSourceStatelessRuleMatchAttributesTcpFlags ::
  RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
newRuleGroupSourceStatelessRuleMatchAttributesTcpFlags =
  RuleGroupSourceStatelessRuleMatchAttributesTcpFlags'
    { flags =
        Prelude.Nothing,
      masks =
        Prelude.Nothing
    }

-- | Defines the flags from the @Masks@ setting that must be set in order for
-- the packet to match. Flags that are listed must be set. Flags that are
-- not listed must not be set.
ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesTcpFlags (Prelude.Maybe [Prelude.Text])
ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {flags} -> flags) (\s@RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {} a -> s {flags = a} :: RuleGroupSourceStatelessRuleMatchAttributesTcpFlags) Prelude.. Lens.mapping Lens.coerced

-- | The set of flags to consider in the inspection. If not specified, then
-- all flags are inspected.
ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesTcpFlags (Prelude.Maybe [Prelude.Text])
ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {masks} -> masks) (\s@RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {} a -> s {masks = a} :: RuleGroupSourceStatelessRuleMatchAttributesTcpFlags) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRuleMatchAttributesTcpFlags"
      ( \x ->
          RuleGroupSourceStatelessRuleMatchAttributesTcpFlags'
            Prelude.<$> (x Data..:? "Flags" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Masks" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {..} =
      _salt `Prelude.hashWithSalt` flags
        `Prelude.hashWithSalt` masks

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
  where
  rnf
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {..} =
      Prelude.rnf flags `Prelude.seq` Prelude.rnf masks

instance
  Data.ToJSON
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
  where
  toJSON
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Flags" Data..=) Prelude.<$> flags,
              ("Masks" Data..=) Prelude.<$> masks
            ]
        )

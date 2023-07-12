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
-- Module      : Amazonka.SecurityHub.Types.WafOverrideAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WafOverrideAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an override action for a rule.
--
-- /See:/ 'newWafOverrideAction' smart constructor.
data WafOverrideAction = WafOverrideAction'
  { -- | @COUNT@ overrides the action specified by the individual rule within a
    -- @RuleGroup@ .
    --
    -- If set to @NONE@, the rule\'s action takes place.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WafOverrideAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'wafOverrideAction_type' - @COUNT@ overrides the action specified by the individual rule within a
-- @RuleGroup@ .
--
-- If set to @NONE@, the rule\'s action takes place.
newWafOverrideAction ::
  WafOverrideAction
newWafOverrideAction =
  WafOverrideAction' {type' = Prelude.Nothing}

-- | @COUNT@ overrides the action specified by the individual rule within a
-- @RuleGroup@ .
--
-- If set to @NONE@, the rule\'s action takes place.
wafOverrideAction_type :: Lens.Lens' WafOverrideAction (Prelude.Maybe Prelude.Text)
wafOverrideAction_type = Lens.lens (\WafOverrideAction' {type'} -> type') (\s@WafOverrideAction' {} a -> s {type' = a} :: WafOverrideAction)

instance Data.FromJSON WafOverrideAction where
  parseJSON =
    Data.withObject
      "WafOverrideAction"
      ( \x ->
          WafOverrideAction' Prelude.<$> (x Data..:? "Type")
      )

instance Prelude.Hashable WafOverrideAction where
  hashWithSalt _salt WafOverrideAction' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData WafOverrideAction where
  rnf WafOverrideAction' {..} = Prelude.rnf type'

instance Data.ToJSON WafOverrideAction where
  toJSON WafOverrideAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Type" Data..=) Prelude.<$> type']
      )

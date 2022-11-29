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
-- Module      : Amazonka.SecurityHub.Types.WafAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WafAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the action that CloudFront or WAF takes when a web request
-- matches the conditions in the rule.
--
-- /See:/ 'newWafAction' smart constructor.
data WafAction = WafAction'
  { -- | Specifies how you want WAF to respond to requests that match the
    -- settings in a rule.
    --
    -- Valid settings include the following:
    --
    -- -   @ALLOW@ - WAF allows requests
    --
    -- -   @BLOCK@ - WAF blocks requests
    --
    -- -   @COUNT@ - WAF increments a counter of the requests that match all of
    --     the conditions in the rule. WAF then continues to inspect the web
    --     request based on the remaining rules in the web ACL. You can\'t
    --     specify @COUNT@ for the default action for a web ACL.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WafAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'wafAction_type' - Specifies how you want WAF to respond to requests that match the
-- settings in a rule.
--
-- Valid settings include the following:
--
-- -   @ALLOW@ - WAF allows requests
--
-- -   @BLOCK@ - WAF blocks requests
--
-- -   @COUNT@ - WAF increments a counter of the requests that match all of
--     the conditions in the rule. WAF then continues to inspect the web
--     request based on the remaining rules in the web ACL. You can\'t
--     specify @COUNT@ for the default action for a web ACL.
newWafAction ::
  WafAction
newWafAction = WafAction' {type' = Prelude.Nothing}

-- | Specifies how you want WAF to respond to requests that match the
-- settings in a rule.
--
-- Valid settings include the following:
--
-- -   @ALLOW@ - WAF allows requests
--
-- -   @BLOCK@ - WAF blocks requests
--
-- -   @COUNT@ - WAF increments a counter of the requests that match all of
--     the conditions in the rule. WAF then continues to inspect the web
--     request based on the remaining rules in the web ACL. You can\'t
--     specify @COUNT@ for the default action for a web ACL.
wafAction_type :: Lens.Lens' WafAction (Prelude.Maybe Prelude.Text)
wafAction_type = Lens.lens (\WafAction' {type'} -> type') (\s@WafAction' {} a -> s {type' = a} :: WafAction)

instance Core.FromJSON WafAction where
  parseJSON =
    Core.withObject
      "WafAction"
      (\x -> WafAction' Prelude.<$> (x Core..:? "Type"))

instance Prelude.Hashable WafAction where
  hashWithSalt _salt WafAction' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData WafAction where
  rnf WafAction' {..} = Prelude.rnf type'

instance Core.ToJSON WafAction where
  toJSON WafAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Type" Core..=) Prelude.<$> type']
      )

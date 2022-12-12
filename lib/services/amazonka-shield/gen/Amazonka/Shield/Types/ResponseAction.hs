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
-- Module      : Amazonka.Shield.Types.ResponseAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ResponseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.BlockAction
import Amazonka.Shield.Types.CountAction

-- | Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
--
-- /See:/ 'newResponseAction' smart constructor.
data ResponseAction = ResponseAction'
  { -- | Specifies that Shield Advanced should configure its WAF rules with the
    -- WAF @Block@ action.
    --
    -- You must specify exactly one action, either @Block@ or @Count@.
    block :: Prelude.Maybe BlockAction,
    -- | Specifies that Shield Advanced should configure its WAF rules with the
    -- WAF @Count@ action.
    --
    -- You must specify exactly one action, either @Block@ or @Count@.
    count :: Prelude.Maybe CountAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'block', 'responseAction_block' - Specifies that Shield Advanced should configure its WAF rules with the
-- WAF @Block@ action.
--
-- You must specify exactly one action, either @Block@ or @Count@.
--
-- 'count', 'responseAction_count' - Specifies that Shield Advanced should configure its WAF rules with the
-- WAF @Count@ action.
--
-- You must specify exactly one action, either @Block@ or @Count@.
newResponseAction ::
  ResponseAction
newResponseAction =
  ResponseAction'
    { block = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Specifies that Shield Advanced should configure its WAF rules with the
-- WAF @Block@ action.
--
-- You must specify exactly one action, either @Block@ or @Count@.
responseAction_block :: Lens.Lens' ResponseAction (Prelude.Maybe BlockAction)
responseAction_block = Lens.lens (\ResponseAction' {block} -> block) (\s@ResponseAction' {} a -> s {block = a} :: ResponseAction)

-- | Specifies that Shield Advanced should configure its WAF rules with the
-- WAF @Count@ action.
--
-- You must specify exactly one action, either @Block@ or @Count@.
responseAction_count :: Lens.Lens' ResponseAction (Prelude.Maybe CountAction)
responseAction_count = Lens.lens (\ResponseAction' {count} -> count) (\s@ResponseAction' {} a -> s {count = a} :: ResponseAction)

instance Data.FromJSON ResponseAction where
  parseJSON =
    Data.withObject
      "ResponseAction"
      ( \x ->
          ResponseAction'
            Prelude.<$> (x Data..:? "Block")
            Prelude.<*> (x Data..:? "Count")
      )

instance Prelude.Hashable ResponseAction where
  hashWithSalt _salt ResponseAction' {..} =
    _salt `Prelude.hashWithSalt` block
      `Prelude.hashWithSalt` count

instance Prelude.NFData ResponseAction where
  rnf ResponseAction' {..} =
    Prelude.rnf block `Prelude.seq` Prelude.rnf count

instance Data.ToJSON ResponseAction where
  toJSON ResponseAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Block" Data..=) Prelude.<$> block,
            ("Count" Data..=) Prelude.<$> count
          ]
      )

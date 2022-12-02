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
-- Module      : Amazonka.WAFV2.Types.DefaultAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.DefaultAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AllowAction
import Amazonka.WAFV2.Types.BlockAction

-- | In a WebACL, this is the action that you want WAF to perform when a web
-- request doesn\'t match any of the rules in the @WebACL@. The default
-- action must be a terminating action.
--
-- /See:/ 'newDefaultAction' smart constructor.
data DefaultAction = DefaultAction'
  { -- | Specifies that WAF should allow requests by default.
    allow :: Prelude.Maybe AllowAction,
    -- | Specifies that WAF should block requests by default.
    block :: Prelude.Maybe BlockAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allow', 'defaultAction_allow' - Specifies that WAF should allow requests by default.
--
-- 'block', 'defaultAction_block' - Specifies that WAF should block requests by default.
newDefaultAction ::
  DefaultAction
newDefaultAction =
  DefaultAction'
    { allow = Prelude.Nothing,
      block = Prelude.Nothing
    }

-- | Specifies that WAF should allow requests by default.
defaultAction_allow :: Lens.Lens' DefaultAction (Prelude.Maybe AllowAction)
defaultAction_allow = Lens.lens (\DefaultAction' {allow} -> allow) (\s@DefaultAction' {} a -> s {allow = a} :: DefaultAction)

-- | Specifies that WAF should block requests by default.
defaultAction_block :: Lens.Lens' DefaultAction (Prelude.Maybe BlockAction)
defaultAction_block = Lens.lens (\DefaultAction' {block} -> block) (\s@DefaultAction' {} a -> s {block = a} :: DefaultAction)

instance Data.FromJSON DefaultAction where
  parseJSON =
    Data.withObject
      "DefaultAction"
      ( \x ->
          DefaultAction'
            Prelude.<$> (x Data..:? "Allow")
            Prelude.<*> (x Data..:? "Block")
      )

instance Prelude.Hashable DefaultAction where
  hashWithSalt _salt DefaultAction' {..} =
    _salt `Prelude.hashWithSalt` allow
      `Prelude.hashWithSalt` block

instance Prelude.NFData DefaultAction where
  rnf DefaultAction' {..} =
    Prelude.rnf allow `Prelude.seq` Prelude.rnf block

instance Data.ToJSON DefaultAction where
  toJSON DefaultAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Allow" Data..=) Prelude.<$> allow,
            ("Block" Data..=) Prelude.<$> block
          ]
      )

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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.AccountLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.AccountLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types.AccountLimitName
import qualified Amazonka.Prelude as Prelude

-- | The current resource quotas associated with an Amazon Web Services
-- account.
--
-- /See:/ 'newAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The name of the attribute to apply the account limit to.
    name :: AccountLimitName,
    -- | The current amount that has been spent, in US dollars.
    used :: Prelude.Integer,
    -- | The Amazon Web Services set limit for that resource type, in US dollars.
    max :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'accountLimit_name' - The name of the attribute to apply the account limit to.
--
-- 'used', 'accountLimit_used' - The current amount that has been spent, in US dollars.
--
-- 'max', 'accountLimit_max' - The Amazon Web Services set limit for that resource type, in US dollars.
newAccountLimit ::
  -- | 'name'
  AccountLimitName ->
  -- | 'used'
  Prelude.Integer ->
  -- | 'max'
  Prelude.Integer ->
  AccountLimit
newAccountLimit pName_ pUsed_ pMax_ =
  AccountLimit'
    { name = pName_,
      used = pUsed_,
      max = pMax_
    }

-- | The name of the attribute to apply the account limit to.
accountLimit_name :: Lens.Lens' AccountLimit AccountLimitName
accountLimit_name = Lens.lens (\AccountLimit' {name} -> name) (\s@AccountLimit' {} a -> s {name = a} :: AccountLimit)

-- | The current amount that has been spent, in US dollars.
accountLimit_used :: Lens.Lens' AccountLimit Prelude.Integer
accountLimit_used = Lens.lens (\AccountLimit' {used} -> used) (\s@AccountLimit' {} a -> s {used = a} :: AccountLimit)

-- | The Amazon Web Services set limit for that resource type, in US dollars.
accountLimit_max :: Lens.Lens' AccountLimit Prelude.Integer
accountLimit_max = Lens.lens (\AccountLimit' {max} -> max) (\s@AccountLimit' {} a -> s {max = a} :: AccountLimit)

instance Core.FromJSON AccountLimit where
  parseJSON =
    Core.withObject
      "AccountLimit"
      ( \x ->
          AccountLimit'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Used")
            Prelude.<*> (x Core..: "Max")
      )

instance Prelude.Hashable AccountLimit where
  hashWithSalt _salt AccountLimit' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` used
      `Prelude.hashWithSalt` max

instance Prelude.NFData AccountLimit where
  rnf AccountLimit' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf used
      `Prelude.seq` Prelude.rnf max

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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.AccountAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.AccountAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types.AccountAttributeName
import qualified Amazonka.Prelude as Prelude

-- | Displays the attributes associated with a single Amazon Web Services
-- account.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The name of the account attribute.
    name :: AccountAttributeName,
    -- | The value associated with the account attribute name.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'accountAttribute_name' - The name of the account attribute.
--
-- 'value', 'accountAttribute_value' - The value associated with the account attribute name.
newAccountAttribute ::
  -- | 'name'
  AccountAttributeName ->
  -- | 'value'
  Prelude.Text ->
  AccountAttribute
newAccountAttribute pName_ pValue_ =
  AccountAttribute' {name = pName_, value = pValue_}

-- | The name of the account attribute.
accountAttribute_name :: Lens.Lens' AccountAttribute AccountAttributeName
accountAttribute_name = Lens.lens (\AccountAttribute' {name} -> name) (\s@AccountAttribute' {} a -> s {name = a} :: AccountAttribute)

-- | The value associated with the account attribute name.
accountAttribute_value :: Lens.Lens' AccountAttribute Prelude.Text
accountAttribute_value = Lens.lens (\AccountAttribute' {value} -> value) (\s@AccountAttribute' {} a -> s {value = a} :: AccountAttribute)

instance Core.FromJSON AccountAttribute where
  parseJSON =
    Core.withObject
      "AccountAttribute"
      ( \x ->
          AccountAttribute'
            Prelude.<$> (x Core..: "Name") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable AccountAttribute where
  hashWithSalt _salt AccountAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData AccountAttribute where
  rnf AccountAttribute' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

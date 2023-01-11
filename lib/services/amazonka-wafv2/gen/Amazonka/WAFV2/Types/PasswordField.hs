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
-- Module      : Amazonka.WAFV2.Types.PasswordField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.PasswordField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about your login page password field, used in a
-- @ManagedRuleGroupConfig@.
--
-- /See:/ 'newPasswordField' smart constructor.
data PasswordField = PasswordField'
  { -- | The name of the password field. For example @\/form\/password@.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PasswordField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'passwordField_identifier' - The name of the password field. For example @\/form\/password@.
newPasswordField ::
  -- | 'identifier'
  Prelude.Text ->
  PasswordField
newPasswordField pIdentifier_ =
  PasswordField' {identifier = pIdentifier_}

-- | The name of the password field. For example @\/form\/password@.
passwordField_identifier :: Lens.Lens' PasswordField Prelude.Text
passwordField_identifier = Lens.lens (\PasswordField' {identifier} -> identifier) (\s@PasswordField' {} a -> s {identifier = a} :: PasswordField)

instance Data.FromJSON PasswordField where
  parseJSON =
    Data.withObject
      "PasswordField"
      ( \x ->
          PasswordField' Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable PasswordField where
  hashWithSalt _salt PasswordField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData PasswordField where
  rnf PasswordField' {..} = Prelude.rnf identifier

instance Data.ToJSON PasswordField where
  toJSON PasswordField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

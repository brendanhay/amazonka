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
-- Module      : Amazonka.WAFV2.Types.UsernameField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.UsernameField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about your login page username field, used in a
-- @ManagedRuleGroupConfig@.
--
-- /See:/ 'newUsernameField' smart constructor.
data UsernameField = UsernameField'
  { -- | The name of the username field. For example @\/form\/username@.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsernameField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'usernameField_identifier' - The name of the username field. For example @\/form\/username@.
newUsernameField ::
  -- | 'identifier'
  Prelude.Text ->
  UsernameField
newUsernameField pIdentifier_ =
  UsernameField' {identifier = pIdentifier_}

-- | The name of the username field. For example @\/form\/username@.
usernameField_identifier :: Lens.Lens' UsernameField Prelude.Text
usernameField_identifier = Lens.lens (\UsernameField' {identifier} -> identifier) (\s@UsernameField' {} a -> s {identifier = a} :: UsernameField)

instance Data.FromJSON UsernameField where
  parseJSON =
    Data.withObject
      "UsernameField"
      ( \x ->
          UsernameField' Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable UsernameField where
  hashWithSalt _salt UsernameField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData UsernameField where
  rnf UsernameField' {..} = Prelude.rnf identifier

instance Data.ToJSON UsernameField where
  toJSON UsernameField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

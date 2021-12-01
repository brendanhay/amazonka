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
-- Module      : Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An LDAP attribute of an Active Directory computer account, in the form
-- of a name:value pair.
--
-- /See:/ 'newActiveDirectoryComputerAttribute' smart constructor.
data ActiveDirectoryComputerAttribute = ActiveDirectoryComputerAttribute'
  { -- | The value for the LDAP attribute.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name for the LDAP attribute.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveDirectoryComputerAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'activeDirectoryComputerAttribute_value' - The value for the LDAP attribute.
--
-- 'name', 'activeDirectoryComputerAttribute_name' - The name for the LDAP attribute.
newActiveDirectoryComputerAttribute ::
  ActiveDirectoryComputerAttribute
newActiveDirectoryComputerAttribute =
  ActiveDirectoryComputerAttribute'
    { value =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value for the LDAP attribute.
activeDirectoryComputerAttribute_value :: Lens.Lens' ActiveDirectoryComputerAttribute (Prelude.Maybe Prelude.Text)
activeDirectoryComputerAttribute_value = Lens.lens (\ActiveDirectoryComputerAttribute' {value} -> value) (\s@ActiveDirectoryComputerAttribute' {} a -> s {value = a} :: ActiveDirectoryComputerAttribute)

-- | The name for the LDAP attribute.
activeDirectoryComputerAttribute_name :: Lens.Lens' ActiveDirectoryComputerAttribute (Prelude.Maybe Prelude.Text)
activeDirectoryComputerAttribute_name = Lens.lens (\ActiveDirectoryComputerAttribute' {name} -> name) (\s@ActiveDirectoryComputerAttribute' {} a -> s {name = a} :: ActiveDirectoryComputerAttribute)

instance
  Core.FromJSON
    ActiveDirectoryComputerAttribute
  where
  parseJSON =
    Core.withObject
      "ActiveDirectoryComputerAttribute"
      ( \x ->
          ActiveDirectoryComputerAttribute'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "name")
      )

instance
  Prelude.Hashable
    ActiveDirectoryComputerAttribute
  where
  hashWithSalt
    salt'
    ActiveDirectoryComputerAttribute' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ActiveDirectoryComputerAttribute
  where
  rnf ActiveDirectoryComputerAttribute' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON ActiveDirectoryComputerAttribute where
  toJSON ActiveDirectoryComputerAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name
          ]
      )

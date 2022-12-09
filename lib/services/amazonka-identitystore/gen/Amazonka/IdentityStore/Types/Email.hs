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
-- Module      : Amazonka.IdentityStore.Types.Email
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.Email where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The email address associated with the user.
--
-- /See:/ 'newEmail' smart constructor.
data Email = Email'
  { -- | A Boolean value representing whether this is the primary email address
    -- for the associated resource.
    primary :: Prelude.Maybe (Data.Sensitive Prelude.Bool),
    -- | A string representing the type of address. For example, \"Work.\"
    type' :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing an email address. For example,
    -- \"johndoe\@amazon.com.\"
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Email' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'email_primary' - A Boolean value representing whether this is the primary email address
-- for the associated resource.
--
-- 'type'', 'email_type' - A string representing the type of address. For example, \"Work.\"
--
-- 'value', 'email_value' - A string containing an email address. For example,
-- \"johndoe\@amazon.com.\"
newEmail ::
  Email
newEmail =
  Email'
    { primary = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A Boolean value representing whether this is the primary email address
-- for the associated resource.
email_primary :: Lens.Lens' Email (Prelude.Maybe Prelude.Bool)
email_primary = Lens.lens (\Email' {primary} -> primary) (\s@Email' {} a -> s {primary = a} :: Email) Prelude.. Lens.mapping Data._Sensitive

-- | A string representing the type of address. For example, \"Work.\"
email_type :: Lens.Lens' Email (Prelude.Maybe Prelude.Text)
email_type = Lens.lens (\Email' {type'} -> type') (\s@Email' {} a -> s {type' = a} :: Email) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing an email address. For example,
-- \"johndoe\@amazon.com.\"
email_value :: Lens.Lens' Email (Prelude.Maybe Prelude.Text)
email_value = Lens.lens (\Email' {value} -> value) (\s@Email' {} a -> s {value = a} :: Email) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Email where
  parseJSON =
    Data.withObject
      "Email"
      ( \x ->
          Email'
            Prelude.<$> (x Data..:? "Primary")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Email where
  hashWithSalt _salt Email' {..} =
    _salt `Prelude.hashWithSalt` primary
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData Email where
  rnf Email' {..} =
    Prelude.rnf primary
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Email where
  toJSON Email' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Primary" Data..=) Prelude.<$> primary,
            ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )

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
-- Module      : Amazonka.Connect.Types.EmailReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EmailReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference when the @referenceType@ is @EMAIL@.
-- Otherwise, null.
--
-- /See:/ 'newEmailReference' smart constructor.
data EmailReference = EmailReference'
  { -- | Identifier of the email reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | A valid email address.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'emailReference_name' - Identifier of the email reference.
--
-- 'value', 'emailReference_value' - A valid email address.
newEmailReference ::
  EmailReference
newEmailReference =
  EmailReference'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the email reference.
emailReference_name :: Lens.Lens' EmailReference (Prelude.Maybe Prelude.Text)
emailReference_name = Lens.lens (\EmailReference' {name} -> name) (\s@EmailReference' {} a -> s {name = a} :: EmailReference)

-- | A valid email address.
emailReference_value :: Lens.Lens' EmailReference (Prelude.Maybe Prelude.Text)
emailReference_value = Lens.lens (\EmailReference' {value} -> value) (\s@EmailReference' {} a -> s {value = a} :: EmailReference)

instance Data.FromJSON EmailReference where
  parseJSON =
    Data.withObject
      "EmailReference"
      ( \x ->
          EmailReference'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable EmailReference where
  hashWithSalt _salt EmailReference' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData EmailReference where
  rnf EmailReference' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

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
-- Module      : Amazonka.Connect.Types.StringReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.StringReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference when the @referenceType@ is @STRING@.
-- Otherwise, null.
--
-- /See:/ 'newStringReference' smart constructor.
data StringReference = StringReference'
  { -- | Identifier of the string reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | A valid string.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stringReference_name' - Identifier of the string reference.
--
-- 'value', 'stringReference_value' - A valid string.
newStringReference ::
  StringReference
newStringReference =
  StringReference'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the string reference.
stringReference_name :: Lens.Lens' StringReference (Prelude.Maybe Prelude.Text)
stringReference_name = Lens.lens (\StringReference' {name} -> name) (\s@StringReference' {} a -> s {name = a} :: StringReference)

-- | A valid string.
stringReference_value :: Lens.Lens' StringReference (Prelude.Maybe Prelude.Text)
stringReference_value = Lens.lens (\StringReference' {value} -> value) (\s@StringReference' {} a -> s {value = a} :: StringReference)

instance Data.FromJSON StringReference where
  parseJSON =
    Data.withObject
      "StringReference"
      ( \x ->
          StringReference'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable StringReference where
  hashWithSalt _salt StringReference' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData StringReference where
  rnf StringReference' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

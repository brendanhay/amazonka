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
-- Module      : Amazonka.Connect.Types.NumberReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.NumberReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference when the @referenceType@ is @NUMBER@.
-- Otherwise, null.
--
-- /See:/ 'newNumberReference' smart constructor.
data NumberReference = NumberReference'
  { -- | Identifier of the number reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | A valid number.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'numberReference_name' - Identifier of the number reference.
--
-- 'value', 'numberReference_value' - A valid number.
newNumberReference ::
  NumberReference
newNumberReference =
  NumberReference'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the number reference.
numberReference_name :: Lens.Lens' NumberReference (Prelude.Maybe Prelude.Text)
numberReference_name = Lens.lens (\NumberReference' {name} -> name) (\s@NumberReference' {} a -> s {name = a} :: NumberReference)

-- | A valid number.
numberReference_value :: Lens.Lens' NumberReference (Prelude.Maybe Prelude.Text)
numberReference_value = Lens.lens (\NumberReference' {value} -> value) (\s@NumberReference' {} a -> s {value = a} :: NumberReference)

instance Data.FromJSON NumberReference where
  parseJSON =
    Data.withObject
      "NumberReference"
      ( \x ->
          NumberReference'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable NumberReference where
  hashWithSalt _salt NumberReference' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData NumberReference where
  rnf NumberReference' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

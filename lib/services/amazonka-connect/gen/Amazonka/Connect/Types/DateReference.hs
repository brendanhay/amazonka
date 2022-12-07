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
-- Module      : Amazonka.Connect.Types.DateReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.DateReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference when the @referenceType@ is @DATE@.
-- Otherwise, null.
--
-- /See:/ 'newDateReference' smart constructor.
data DateReference = DateReference'
  { -- | Identifier of the date reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | A valid date.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dateReference_name' - Identifier of the date reference.
--
-- 'value', 'dateReference_value' - A valid date.
newDateReference ::
  DateReference
newDateReference =
  DateReference'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the date reference.
dateReference_name :: Lens.Lens' DateReference (Prelude.Maybe Prelude.Text)
dateReference_name = Lens.lens (\DateReference' {name} -> name) (\s@DateReference' {} a -> s {name = a} :: DateReference)

-- | A valid date.
dateReference_value :: Lens.Lens' DateReference (Prelude.Maybe Prelude.Text)
dateReference_value = Lens.lens (\DateReference' {value} -> value) (\s@DateReference' {} a -> s {value = a} :: DateReference)

instance Data.FromJSON DateReference where
  parseJSON =
    Data.withObject
      "DateReference"
      ( \x ->
          DateReference'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable DateReference where
  hashWithSalt _salt DateReference' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData DateReference where
  rnf DateReference' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

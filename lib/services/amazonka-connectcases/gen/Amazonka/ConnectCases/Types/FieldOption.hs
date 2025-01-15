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
-- Module      : Amazonka.ConnectCases.Types.FieldOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for field Options information.
--
-- /See:/ 'newFieldOption' smart constructor.
data FieldOption = FieldOption'
  { -- | Describes whether the @FieldOption@ is active (displayed) or inactive.
    active :: Prelude.Bool,
    -- | @FieldOptionName@ has max length 100 and disallows trailing spaces.
    name :: Prelude.Text,
    -- | @FieldOptionValue@ has max length 100 and must be alphanumeric with
    -- hyphens and underscores.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'fieldOption_active' - Describes whether the @FieldOption@ is active (displayed) or inactive.
--
-- 'name', 'fieldOption_name' - @FieldOptionName@ has max length 100 and disallows trailing spaces.
--
-- 'value', 'fieldOption_value' - @FieldOptionValue@ has max length 100 and must be alphanumeric with
-- hyphens and underscores.
newFieldOption ::
  -- | 'active'
  Prelude.Bool ->
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  FieldOption
newFieldOption pActive_ pName_ pValue_ =
  FieldOption'
    { active = pActive_,
      name = pName_,
      value = pValue_
    }

-- | Describes whether the @FieldOption@ is active (displayed) or inactive.
fieldOption_active :: Lens.Lens' FieldOption Prelude.Bool
fieldOption_active = Lens.lens (\FieldOption' {active} -> active) (\s@FieldOption' {} a -> s {active = a} :: FieldOption)

-- | @FieldOptionName@ has max length 100 and disallows trailing spaces.
fieldOption_name :: Lens.Lens' FieldOption Prelude.Text
fieldOption_name = Lens.lens (\FieldOption' {name} -> name) (\s@FieldOption' {} a -> s {name = a} :: FieldOption)

-- | @FieldOptionValue@ has max length 100 and must be alphanumeric with
-- hyphens and underscores.
fieldOption_value :: Lens.Lens' FieldOption Prelude.Text
fieldOption_value = Lens.lens (\FieldOption' {value} -> value) (\s@FieldOption' {} a -> s {value = a} :: FieldOption)

instance Data.FromJSON FieldOption where
  parseJSON =
    Data.withObject
      "FieldOption"
      ( \x ->
          FieldOption'
            Prelude.<$> (x Data..: "active")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable FieldOption where
  hashWithSalt _salt FieldOption' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData FieldOption where
  rnf FieldOption' {..} =
    Prelude.rnf active `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf value

instance Data.ToJSON FieldOption where
  toJSON FieldOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("active" Data..= active),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )

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
-- Module      : Amazonka.ImageBuilder.Types.ComponentParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ComponentParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a key\/value pair that sets the named component parameter.
--
-- /See:/ 'newComponentParameter' smart constructor.
data ComponentParameter = ComponentParameter'
  { -- | The name of the component parameter to set.
    name :: Prelude.Text,
    -- | Sets the value for the named component parameter.
    value :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'componentParameter_name' - The name of the component parameter to set.
--
-- 'value', 'componentParameter_value' - Sets the value for the named component parameter.
newComponentParameter ::
  -- | 'name'
  Prelude.Text ->
  ComponentParameter
newComponentParameter pName_ =
  ComponentParameter'
    { name = pName_,
      value = Prelude.mempty
    }

-- | The name of the component parameter to set.
componentParameter_name :: Lens.Lens' ComponentParameter Prelude.Text
componentParameter_name = Lens.lens (\ComponentParameter' {name} -> name) (\s@ComponentParameter' {} a -> s {name = a} :: ComponentParameter)

-- | Sets the value for the named component parameter.
componentParameter_value :: Lens.Lens' ComponentParameter [Prelude.Text]
componentParameter_value = Lens.lens (\ComponentParameter' {value} -> value) (\s@ComponentParameter' {} a -> s {value = a} :: ComponentParameter) Prelude.. Lens.coerced

instance Data.FromJSON ComponentParameter where
  parseJSON =
    Data.withObject
      "ComponentParameter"
      ( \x ->
          ComponentParameter'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..:? "value" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ComponentParameter where
  hashWithSalt _salt ComponentParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ComponentParameter where
  rnf ComponentParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ComponentParameter where
  toJSON ComponentParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )

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
-- Module      : Amazonka.ImageBuilder.Types.ComponentParameterDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ComponentParameterDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a parameter that is used to provide configuration details for
-- the component.
--
-- /See:/ 'newComponentParameterDetail' smart constructor.
data ComponentParameterDetail = ComponentParameterDetail'
  { -- | The default value of this parameter if no input is provided.
    defaultValue :: Prelude.Maybe [Prelude.Text],
    -- | Describes this parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of this input parameter.
    name :: Prelude.Text,
    -- | The type of input this parameter provides. The currently supported value
    -- is \"string\".
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentParameterDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'componentParameterDetail_defaultValue' - The default value of this parameter if no input is provided.
--
-- 'description', 'componentParameterDetail_description' - Describes this parameter.
--
-- 'name', 'componentParameterDetail_name' - The name of this input parameter.
--
-- 'type'', 'componentParameterDetail_type' - The type of input this parameter provides. The currently supported value
-- is \"string\".
newComponentParameterDetail ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  ComponentParameterDetail
newComponentParameterDetail pName_ pType_ =
  ComponentParameterDetail'
    { defaultValue =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The default value of this parameter if no input is provided.
componentParameterDetail_defaultValue :: Lens.Lens' ComponentParameterDetail (Prelude.Maybe [Prelude.Text])
componentParameterDetail_defaultValue = Lens.lens (\ComponentParameterDetail' {defaultValue} -> defaultValue) (\s@ComponentParameterDetail' {} a -> s {defaultValue = a} :: ComponentParameterDetail) Prelude.. Lens.mapping Lens.coerced

-- | Describes this parameter.
componentParameterDetail_description :: Lens.Lens' ComponentParameterDetail (Prelude.Maybe Prelude.Text)
componentParameterDetail_description = Lens.lens (\ComponentParameterDetail' {description} -> description) (\s@ComponentParameterDetail' {} a -> s {description = a} :: ComponentParameterDetail)

-- | The name of this input parameter.
componentParameterDetail_name :: Lens.Lens' ComponentParameterDetail Prelude.Text
componentParameterDetail_name = Lens.lens (\ComponentParameterDetail' {name} -> name) (\s@ComponentParameterDetail' {} a -> s {name = a} :: ComponentParameterDetail)

-- | The type of input this parameter provides. The currently supported value
-- is \"string\".
componentParameterDetail_type :: Lens.Lens' ComponentParameterDetail Prelude.Text
componentParameterDetail_type = Lens.lens (\ComponentParameterDetail' {type'} -> type') (\s@ComponentParameterDetail' {} a -> s {type' = a} :: ComponentParameterDetail)

instance Data.FromJSON ComponentParameterDetail where
  parseJSON =
    Data.withObject
      "ComponentParameterDetail"
      ( \x ->
          ComponentParameterDetail'
            Prelude.<$> (x Data..:? "defaultValue" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ComponentParameterDetail where
  hashWithSalt _salt ComponentParameterDetail' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ComponentParameterDetail where
  rnf ComponentParameterDetail' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

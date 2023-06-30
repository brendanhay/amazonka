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
-- Module      : Amazonka.TimeStreamQuery.Types.ParameterMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ParameterMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.Type

-- | Mapping for named parameters.
--
-- /See:/ 'newParameterMapping' smart constructor.
data ParameterMapping = ParameterMapping'
  { -- | Parameter name.
    name :: Prelude.Text,
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parameterMapping_name' - Parameter name.
--
-- 'type'', 'parameterMapping_type' - Undocumented member.
newParameterMapping ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Type ->
  ParameterMapping
newParameterMapping pName_ pType_ =
  ParameterMapping' {name = pName_, type' = pType_}

-- | Parameter name.
parameterMapping_name :: Lens.Lens' ParameterMapping Prelude.Text
parameterMapping_name = Lens.lens (\ParameterMapping' {name} -> name) (\s@ParameterMapping' {} a -> s {name = a} :: ParameterMapping)

-- | Undocumented member.
parameterMapping_type :: Lens.Lens' ParameterMapping Type
parameterMapping_type = Lens.lens (\ParameterMapping' {type'} -> type') (\s@ParameterMapping' {} a -> s {type' = a} :: ParameterMapping)

instance Data.FromJSON ParameterMapping where
  parseJSON =
    Data.withObject
      "ParameterMapping"
      ( \x ->
          ParameterMapping'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable ParameterMapping where
  hashWithSalt _salt ParameterMapping' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ParameterMapping where
  rnf ParameterMapping' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

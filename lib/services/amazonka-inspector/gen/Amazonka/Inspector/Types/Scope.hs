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
-- Module      : Amazonka.Inspector.Types.Scope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.Scope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.ScopeType
import qualified Amazonka.Prelude as Prelude

-- | This data type contains key-value pairs that identify various Amazon
-- resources.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The type of the scope.
    key :: Prelude.Maybe ScopeType,
    -- | The resource identifier for the specified scope type.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'scope_key' - The type of the scope.
--
-- 'value', 'scope_value' - The resource identifier for the specified scope type.
newScope ::
  Scope
newScope =
  Scope'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of the scope.
scope_key :: Lens.Lens' Scope (Prelude.Maybe ScopeType)
scope_key = Lens.lens (\Scope' {key} -> key) (\s@Scope' {} a -> s {key = a} :: Scope)

-- | The resource identifier for the specified scope type.
scope_value :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_value = Lens.lens (\Scope' {value} -> value) (\s@Scope' {} a -> s {value = a} :: Scope)

instance Data.FromJSON Scope where
  parseJSON =
    Data.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Data..:? "key") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Scope where
  hashWithSalt _salt Scope' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Scope where
  rnf Scope' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

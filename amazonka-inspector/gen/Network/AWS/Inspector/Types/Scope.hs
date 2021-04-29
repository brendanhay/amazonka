{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Inspector.Types.Scope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Scope where

import Network.AWS.Inspector.Types.ScopeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Scope where
  parseJSON =
    Prelude.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Prelude..:? "key")
            Prelude.<*> (x Prelude..:? "value")
      )

instance Prelude.Hashable Scope

instance Prelude.NFData Scope

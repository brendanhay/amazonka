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
-- Module      : Network.AWS.XRay.Types.Alias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Alias where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An alias for an edge.
--
-- /See:/ 'newAlias' smart constructor.
data Alias = Alias'
  { -- | A list of names for the alias, including the canonical name.
    names :: Core.Maybe [Core.Text],
    -- | The canonical name of the alias.
    name :: Core.Maybe Core.Text,
    -- | The type of the alias.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Alias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'alias_names' - A list of names for the alias, including the canonical name.
--
-- 'name', 'alias_name' - The canonical name of the alias.
--
-- 'type'', 'alias_type' - The type of the alias.
newAlias ::
  Alias
newAlias =
  Alias'
    { names = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- | A list of names for the alias, including the canonical name.
alias_names :: Lens.Lens' Alias (Core.Maybe [Core.Text])
alias_names = Lens.lens (\Alias' {names} -> names) (\s@Alias' {} a -> s {names = a} :: Alias) Core.. Lens.mapping Lens._Coerce

-- | The canonical name of the alias.
alias_name :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_name = Lens.lens (\Alias' {name} -> name) (\s@Alias' {} a -> s {name = a} :: Alias)

-- | The type of the alias.
alias_type :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_type = Lens.lens (\Alias' {type'} -> type') (\s@Alias' {} a -> s {type' = a} :: Alias)

instance Core.FromJSON Alias where
  parseJSON =
    Core.withObject
      "Alias"
      ( \x ->
          Alias'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Alias

instance Core.NFData Alias

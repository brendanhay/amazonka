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
-- Module      : Network.AWS.ElastiCache.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Filter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Used to streamline results of a search based on the property being
-- filtered.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The property being filtered. For example, UserId.
    name :: Core.Text,
    -- | The property values to filter on. For example, \"user-123\".
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The property being filtered. For example, UserId.
--
-- 'values', 'filter_values' - The property values to filter on. For example, \"user-123\".
newFilter ::
  -- | 'name'
  Core.Text ->
  -- | 'values'
  Core.NonEmpty Core.Text ->
  Filter
newFilter pName_ pValues_ =
  Filter'
    { name = pName_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | The property being filtered. For example, UserId.
filter_name :: Lens.Lens' Filter Core.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The property values to filter on. For example, \"user-123\".
filter_values :: Lens.Lens' Filter (Core.NonEmpty Core.Text)
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Core.. Lens._Coerce

instance Core.Hashable Filter

instance Core.NFData Filter

instance Core.ToQuery Filter where
  toQuery Filter' {..} =
    Core.mconcat
      [ "Name" Core.=: name,
        "Values" Core.=: Core.toQueryList "member" values
      ]

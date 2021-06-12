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
-- Module      : Network.AWS.GuardDuty.Types.SortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.SortCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.OrderBy
import qualified Network.AWS.Lens as Lens

-- | Contains information about the criteria used for sorting findings.
--
-- /See:/ 'newSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { -- | Represents the finding attribute (for example, accountId) to sort
    -- findings by.
    attributeName :: Core.Maybe Core.Text,
    -- | The order by which the sorted findings are to be displayed.
    orderBy :: Core.Maybe OrderBy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'sortCriteria_attributeName' - Represents the finding attribute (for example, accountId) to sort
-- findings by.
--
-- 'orderBy', 'sortCriteria_orderBy' - The order by which the sorted findings are to be displayed.
newSortCriteria ::
  SortCriteria
newSortCriteria =
  SortCriteria'
    { attributeName = Core.Nothing,
      orderBy = Core.Nothing
    }

-- | Represents the finding attribute (for example, accountId) to sort
-- findings by.
sortCriteria_attributeName :: Lens.Lens' SortCriteria (Core.Maybe Core.Text)
sortCriteria_attributeName = Lens.lens (\SortCriteria' {attributeName} -> attributeName) (\s@SortCriteria' {} a -> s {attributeName = a} :: SortCriteria)

-- | The order by which the sorted findings are to be displayed.
sortCriteria_orderBy :: Lens.Lens' SortCriteria (Core.Maybe OrderBy)
sortCriteria_orderBy = Lens.lens (\SortCriteria' {orderBy} -> orderBy) (\s@SortCriteria' {} a -> s {orderBy = a} :: SortCriteria)

instance Core.Hashable SortCriteria

instance Core.NFData SortCriteria

instance Core.ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("attributeName" Core..=) Core.<$> attributeName,
            ("orderBy" Core..=) Core.<$> orderBy
          ]
      )

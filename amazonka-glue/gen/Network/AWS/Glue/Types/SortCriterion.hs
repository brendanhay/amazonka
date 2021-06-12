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
-- Module      : Network.AWS.Glue.Types.SortCriterion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SortCriterion where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Sort
import qualified Network.AWS.Lens as Lens

-- | Specifies a field to sort by and a sort order.
--
-- /See:/ 'newSortCriterion' smart constructor.
data SortCriterion = SortCriterion'
  { -- | The name of the field on which to sort.
    fieldName :: Core.Maybe Core.Text,
    -- | An ascending or descending sort.
    sort :: Core.Maybe Sort
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SortCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldName', 'sortCriterion_fieldName' - The name of the field on which to sort.
--
-- 'sort', 'sortCriterion_sort' - An ascending or descending sort.
newSortCriterion ::
  SortCriterion
newSortCriterion =
  SortCriterion'
    { fieldName = Core.Nothing,
      sort = Core.Nothing
    }

-- | The name of the field on which to sort.
sortCriterion_fieldName :: Lens.Lens' SortCriterion (Core.Maybe Core.Text)
sortCriterion_fieldName = Lens.lens (\SortCriterion' {fieldName} -> fieldName) (\s@SortCriterion' {} a -> s {fieldName = a} :: SortCriterion)

-- | An ascending or descending sort.
sortCriterion_sort :: Lens.Lens' SortCriterion (Core.Maybe Sort)
sortCriterion_sort = Lens.lens (\SortCriterion' {sort} -> sort) (\s@SortCriterion' {} a -> s {sort = a} :: SortCriterion)

instance Core.Hashable SortCriterion

instance Core.NFData SortCriterion

instance Core.ToJSON SortCriterion where
  toJSON SortCriterion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FieldName" Core..=) Core.<$> fieldName,
            ("Sort" Core..=) Core.<$> sort
          ]
      )

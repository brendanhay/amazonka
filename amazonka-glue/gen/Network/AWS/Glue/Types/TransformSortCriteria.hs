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
-- Module      : Network.AWS.Glue.Types.TransformSortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortCriteria where

import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TransformSortColumnType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The sorting criteria that are associated with the machine learning
-- transform.
--
-- /See:/ 'newTransformSortCriteria' smart constructor.
data TransformSortCriteria = TransformSortCriteria'
  { -- | The column to be used in the sorting criteria that are associated with
    -- the machine learning transform.
    column :: TransformSortColumnType,
    -- | The sort direction to be used in the sorting criteria that are
    -- associated with the machine learning transform.
    sortDirection :: SortDirectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransformSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'transformSortCriteria_column' - The column to be used in the sorting criteria that are associated with
-- the machine learning transform.
--
-- 'sortDirection', 'transformSortCriteria_sortDirection' - The sort direction to be used in the sorting criteria that are
-- associated with the machine learning transform.
newTransformSortCriteria ::
  -- | 'column'
  TransformSortColumnType ->
  -- | 'sortDirection'
  SortDirectionType ->
  TransformSortCriteria
newTransformSortCriteria pColumn_ pSortDirection_ =
  TransformSortCriteria'
    { column = pColumn_,
      sortDirection = pSortDirection_
    }

-- | The column to be used in the sorting criteria that are associated with
-- the machine learning transform.
transformSortCriteria_column :: Lens.Lens' TransformSortCriteria TransformSortColumnType
transformSortCriteria_column = Lens.lens (\TransformSortCriteria' {column} -> column) (\s@TransformSortCriteria' {} a -> s {column = a} :: TransformSortCriteria)

-- | The sort direction to be used in the sorting criteria that are
-- associated with the machine learning transform.
transformSortCriteria_sortDirection :: Lens.Lens' TransformSortCriteria SortDirectionType
transformSortCriteria_sortDirection = Lens.lens (\TransformSortCriteria' {sortDirection} -> sortDirection) (\s@TransformSortCriteria' {} a -> s {sortDirection = a} :: TransformSortCriteria)

instance Prelude.Hashable TransformSortCriteria

instance Prelude.NFData TransformSortCriteria

instance Prelude.ToJSON TransformSortCriteria where
  toJSON TransformSortCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Prelude..= column),
            Prelude.Just
              ("SortDirection" Prelude..= sortDirection)
          ]
      )

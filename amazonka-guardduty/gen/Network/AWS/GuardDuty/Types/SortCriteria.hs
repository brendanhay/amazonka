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
-- Module      : Network.AWS.GuardDuty.Types.SortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.SortCriteria where

import Network.AWS.GuardDuty.Types.OrderBy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the criteria used for sorting findings.
--
-- /See:/ 'newSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { -- | Represents the finding attribute (for example, accountId) to sort
    -- findings by.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The order by which the sorted findings are to be displayed.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { attributeName = Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | Represents the finding attribute (for example, accountId) to sort
-- findings by.
sortCriteria_attributeName :: Lens.Lens' SortCriteria (Prelude.Maybe Prelude.Text)
sortCriteria_attributeName = Lens.lens (\SortCriteria' {attributeName} -> attributeName) (\s@SortCriteria' {} a -> s {attributeName = a} :: SortCriteria)

-- | The order by which the sorted findings are to be displayed.
sortCriteria_orderBy :: Lens.Lens' SortCriteria (Prelude.Maybe OrderBy)
sortCriteria_orderBy = Lens.lens (\SortCriteria' {orderBy} -> orderBy) (\s@SortCriteria' {} a -> s {orderBy = a} :: SortCriteria)

instance Prelude.Hashable SortCriteria

instance Prelude.NFData SortCriteria

instance Prelude.ToJSON SortCriteria where
  toJSON SortCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("attributeName" Prelude..=)
              Prelude.<$> attributeName,
            ("orderBy" Prelude..=) Prelude.<$> orderBy
          ]
      )

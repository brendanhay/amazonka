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
-- Module      : Amazonka.MacieV2.Types.ListJobsSortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ListJobsSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.ListJobsSortAttributeName
import Amazonka.MacieV2.Types.OrderBy
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a request for information
-- about classification jobs.
--
-- /See:/ 'newListJobsSortCriteria' smart constructor.
data ListJobsSortCriteria = ListJobsSortCriteria'
  { -- | The property to sort the results by.
    attributeName :: Prelude.Maybe ListJobsSortAttributeName,
    -- | The sort order to apply to the results, based on the value for the
    -- property specified by the attributeName property. Valid values are: ASC,
    -- sort the results in ascending order; and, DESC, sort the results in
    -- descending order.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'listJobsSortCriteria_attributeName' - The property to sort the results by.
--
-- 'orderBy', 'listJobsSortCriteria_orderBy' - The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
newListJobsSortCriteria ::
  ListJobsSortCriteria
newListJobsSortCriteria =
  ListJobsSortCriteria'
    { attributeName =
        Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | The property to sort the results by.
listJobsSortCriteria_attributeName :: Lens.Lens' ListJobsSortCriteria (Prelude.Maybe ListJobsSortAttributeName)
listJobsSortCriteria_attributeName = Lens.lens (\ListJobsSortCriteria' {attributeName} -> attributeName) (\s@ListJobsSortCriteria' {} a -> s {attributeName = a} :: ListJobsSortCriteria)

-- | The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
listJobsSortCriteria_orderBy :: Lens.Lens' ListJobsSortCriteria (Prelude.Maybe OrderBy)
listJobsSortCriteria_orderBy = Lens.lens (\ListJobsSortCriteria' {orderBy} -> orderBy) (\s@ListJobsSortCriteria' {} a -> s {orderBy = a} :: ListJobsSortCriteria)

instance Prelude.Hashable ListJobsSortCriteria where
  hashWithSalt _salt ListJobsSortCriteria' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData ListJobsSortCriteria where
  rnf ListJobsSortCriteria' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON ListJobsSortCriteria where
  toJSON ListJobsSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeName" Data..=) Prelude.<$> attributeName,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )

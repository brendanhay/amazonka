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
-- Module      : Amazonka.Connect.Types.HoursOfOperationSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newHoursOfOperationSearchFilter' smart constructor.
data HoursOfOperationSearchFilter = HoursOfOperationSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoursOfOperationSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'hoursOfOperationSearchFilter_tagFilter' - Undocumented member.
newHoursOfOperationSearchFilter ::
  HoursOfOperationSearchFilter
newHoursOfOperationSearchFilter =
  HoursOfOperationSearchFilter'
    { tagFilter =
        Prelude.Nothing
    }

-- | Undocumented member.
hoursOfOperationSearchFilter_tagFilter :: Lens.Lens' HoursOfOperationSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
hoursOfOperationSearchFilter_tagFilter = Lens.lens (\HoursOfOperationSearchFilter' {tagFilter} -> tagFilter) (\s@HoursOfOperationSearchFilter' {} a -> s {tagFilter = a} :: HoursOfOperationSearchFilter)

instance
  Prelude.Hashable
    HoursOfOperationSearchFilter
  where
  hashWithSalt _salt HoursOfOperationSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData HoursOfOperationSearchFilter where
  rnf HoursOfOperationSearchFilter' {..} =
    Prelude.rnf tagFilter

instance Data.ToJSON HoursOfOperationSearchFilter where
  toJSON HoursOfOperationSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TagFilter" Data..=) Prelude.<$> tagFilter]
      )

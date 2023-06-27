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
-- Module      : Amazonka.Connect.Types.QuickConnectSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnectSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newQuickConnectSearchFilter' smart constructor.
data QuickConnectSearchFilter = QuickConnectSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuickConnectSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'quickConnectSearchFilter_tagFilter' - Undocumented member.
newQuickConnectSearchFilter ::
  QuickConnectSearchFilter
newQuickConnectSearchFilter =
  QuickConnectSearchFilter'
    { tagFilter =
        Prelude.Nothing
    }

-- | Undocumented member.
quickConnectSearchFilter_tagFilter :: Lens.Lens' QuickConnectSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
quickConnectSearchFilter_tagFilter = Lens.lens (\QuickConnectSearchFilter' {tagFilter} -> tagFilter) (\s@QuickConnectSearchFilter' {} a -> s {tagFilter = a} :: QuickConnectSearchFilter)

instance Prelude.Hashable QuickConnectSearchFilter where
  hashWithSalt _salt QuickConnectSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData QuickConnectSearchFilter where
  rnf QuickConnectSearchFilter' {..} =
    Prelude.rnf tagFilter

instance Data.ToJSON QuickConnectSearchFilter where
  toJSON QuickConnectSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TagFilter" Data..=) Prelude.<$> tagFilter]
      )

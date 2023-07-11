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
-- Module      : Amazonka.Connect.Types.RoutingProfileSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newRoutingProfileSearchFilter' smart constructor.
data RoutingProfileSearchFilter = RoutingProfileSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'routingProfileSearchFilter_tagFilter' - Undocumented member.
newRoutingProfileSearchFilter ::
  RoutingProfileSearchFilter
newRoutingProfileSearchFilter =
  RoutingProfileSearchFilter'
    { tagFilter =
        Prelude.Nothing
    }

-- | Undocumented member.
routingProfileSearchFilter_tagFilter :: Lens.Lens' RoutingProfileSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
routingProfileSearchFilter_tagFilter = Lens.lens (\RoutingProfileSearchFilter' {tagFilter} -> tagFilter) (\s@RoutingProfileSearchFilter' {} a -> s {tagFilter = a} :: RoutingProfileSearchFilter)

instance Prelude.Hashable RoutingProfileSearchFilter where
  hashWithSalt _salt RoutingProfileSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData RoutingProfileSearchFilter where
  rnf RoutingProfileSearchFilter' {..} =
    Prelude.rnf tagFilter

instance Data.ToJSON RoutingProfileSearchFilter where
  toJSON RoutingProfileSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TagFilter" Data..=) Prelude.<$> tagFilter]
      )

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
-- Module      : Amazonka.Connect.Types.UserSearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newUserSearchFilter' smart constructor.
data UserSearchFilter = UserSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'userSearchFilter_tagFilter' - Undocumented member.
newUserSearchFilter ::
  UserSearchFilter
newUserSearchFilter =
  UserSearchFilter' {tagFilter = Prelude.Nothing}

-- | Undocumented member.
userSearchFilter_tagFilter :: Lens.Lens' UserSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
userSearchFilter_tagFilter = Lens.lens (\UserSearchFilter' {tagFilter} -> tagFilter) (\s@UserSearchFilter' {} a -> s {tagFilter = a} :: UserSearchFilter)

instance Prelude.Hashable UserSearchFilter where
  hashWithSalt _salt UserSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData UserSearchFilter where
  rnf UserSearchFilter' {..} = Prelude.rnf tagFilter

instance Data.ToJSON UserSearchFilter where
  toJSON UserSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TagFilter" Data..=) Prelude.<$> tagFilter]
      )

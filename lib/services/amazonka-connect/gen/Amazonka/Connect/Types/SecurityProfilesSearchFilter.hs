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
-- Module      : Amazonka.Connect.Types.SecurityProfilesSearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SecurityProfilesSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newSecurityProfilesSearchFilter' smart constructor.
data SecurityProfilesSearchFilter = SecurityProfilesSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfilesSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'securityProfilesSearchFilter_tagFilter' - Undocumented member.
newSecurityProfilesSearchFilter ::
  SecurityProfilesSearchFilter
newSecurityProfilesSearchFilter =
  SecurityProfilesSearchFilter'
    { tagFilter =
        Prelude.Nothing
    }

-- | Undocumented member.
securityProfilesSearchFilter_tagFilter :: Lens.Lens' SecurityProfilesSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
securityProfilesSearchFilter_tagFilter = Lens.lens (\SecurityProfilesSearchFilter' {tagFilter} -> tagFilter) (\s@SecurityProfilesSearchFilter' {} a -> s {tagFilter = a} :: SecurityProfilesSearchFilter)

instance
  Prelude.Hashable
    SecurityProfilesSearchFilter
  where
  hashWithSalt _salt SecurityProfilesSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData SecurityProfilesSearchFilter where
  rnf SecurityProfilesSearchFilter' {..} =
    Prelude.rnf tagFilter

instance Core.ToJSON SecurityProfilesSearchFilter where
  toJSON SecurityProfilesSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [("TagFilter" Core..=) Prelude.<$> tagFilter]
      )

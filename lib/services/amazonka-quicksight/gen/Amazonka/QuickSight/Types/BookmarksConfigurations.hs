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
-- Module      : Amazonka.QuickSight.Types.BookmarksConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BookmarksConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The bookmarks configuration of an embedded dashboard.
--
-- /See:/ 'newBookmarksConfigurations' smart constructor.
data BookmarksConfigurations = BookmarksConfigurations'
  { -- | A Boolean value that determines whether a user can bookmark an embedded
    -- dashboard.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BookmarksConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'bookmarksConfigurations_enabled' - A Boolean value that determines whether a user can bookmark an embedded
-- dashboard.
newBookmarksConfigurations ::
  -- | 'enabled'
  Prelude.Bool ->
  BookmarksConfigurations
newBookmarksConfigurations pEnabled_ =
  BookmarksConfigurations' {enabled = pEnabled_}

-- | A Boolean value that determines whether a user can bookmark an embedded
-- dashboard.
bookmarksConfigurations_enabled :: Lens.Lens' BookmarksConfigurations Prelude.Bool
bookmarksConfigurations_enabled = Lens.lens (\BookmarksConfigurations' {enabled} -> enabled) (\s@BookmarksConfigurations' {} a -> s {enabled = a} :: BookmarksConfigurations)

instance Prelude.Hashable BookmarksConfigurations where
  hashWithSalt _salt BookmarksConfigurations' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData BookmarksConfigurations where
  rnf BookmarksConfigurations' {..} =
    Prelude.rnf enabled

instance Data.ToJSON BookmarksConfigurations where
  toJSON BookmarksConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Enabled" Data..= enabled)]
      )

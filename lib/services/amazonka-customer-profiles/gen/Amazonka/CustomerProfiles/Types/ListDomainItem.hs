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
-- Module      : Amazonka.CustomerProfiles.Types.ListDomainItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListDomainItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object in a list that represents a domain.
--
-- /See:/ 'newListDomainItem' smart constructor.
data ListDomainItem = ListDomainItem'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The timestamp of when the domain was created.
    createdAt :: Core.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listDomainItem_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'listDomainItem_domainName' - The unique name of the domain.
--
-- 'createdAt', 'listDomainItem_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'listDomainItem_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newListDomainItem ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  ListDomainItem
newListDomainItem
  pDomainName_
  pCreatedAt_
  pLastUpdatedAt_ =
    ListDomainItem'
      { tags = Prelude.Nothing,
        domainName = pDomainName_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Core._Time Lens.# pLastUpdatedAt_
      }

-- | The tags used to organize, track, or control access for this resource.
listDomainItem_tags :: Lens.Lens' ListDomainItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listDomainItem_tags = Lens.lens (\ListDomainItem' {tags} -> tags) (\s@ListDomainItem' {} a -> s {tags = a} :: ListDomainItem) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
listDomainItem_domainName :: Lens.Lens' ListDomainItem Prelude.Text
listDomainItem_domainName = Lens.lens (\ListDomainItem' {domainName} -> domainName) (\s@ListDomainItem' {} a -> s {domainName = a} :: ListDomainItem)

-- | The timestamp of when the domain was created.
listDomainItem_createdAt :: Lens.Lens' ListDomainItem Prelude.UTCTime
listDomainItem_createdAt = Lens.lens (\ListDomainItem' {createdAt} -> createdAt) (\s@ListDomainItem' {} a -> s {createdAt = a} :: ListDomainItem) Prelude.. Core._Time

-- | The timestamp of when the domain was most recently edited.
listDomainItem_lastUpdatedAt :: Lens.Lens' ListDomainItem Prelude.UTCTime
listDomainItem_lastUpdatedAt = Lens.lens (\ListDomainItem' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListDomainItem' {} a -> s {lastUpdatedAt = a} :: ListDomainItem) Prelude.. Core._Time

instance Core.FromJSON ListDomainItem where
  parseJSON =
    Core.withObject
      "ListDomainItem"
      ( \x ->
          ListDomainItem'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "DomainName")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "LastUpdatedAt")
      )

instance Prelude.Hashable ListDomainItem where
  hashWithSalt _salt ListDomainItem' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt

instance Prelude.NFData ListDomainItem where
  rnf ListDomainItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt

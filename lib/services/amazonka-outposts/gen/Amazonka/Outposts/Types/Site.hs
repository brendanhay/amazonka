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
-- Module      : Amazonka.Outposts.Types.Site
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Site where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a site.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { accountId :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    siteId :: Prelude.Maybe Prelude.Text,
    siteArn :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    -- | The site tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Site' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'site_accountId' - Undocumented member.
--
-- 'name', 'site_name' - Undocumented member.
--
-- 'siteId', 'site_siteId' - Undocumented member.
--
-- 'siteArn', 'site_siteArn' - Undocumented member.
--
-- 'description', 'site_description' - Undocumented member.
--
-- 'tags', 'site_tags' - The site tags.
newSite ::
  Site
newSite =
  Site'
    { accountId = Prelude.Nothing,
      name = Prelude.Nothing,
      siteId = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
site_accountId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_accountId = Lens.lens (\Site' {accountId} -> accountId) (\s@Site' {} a -> s {accountId = a} :: Site)

-- | Undocumented member.
site_name :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_name = Lens.lens (\Site' {name} -> name) (\s@Site' {} a -> s {name = a} :: Site)

-- | Undocumented member.
site_siteId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteId = Lens.lens (\Site' {siteId} -> siteId) (\s@Site' {} a -> s {siteId = a} :: Site)

-- | Undocumented member.
site_siteArn :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteArn = Lens.lens (\Site' {siteArn} -> siteArn) (\s@Site' {} a -> s {siteArn = a} :: Site)

-- | Undocumented member.
site_description :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_description = Lens.lens (\Site' {description} -> description) (\s@Site' {} a -> s {description = a} :: Site)

-- | The site tags.
site_tags :: Lens.Lens' Site (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
site_tags = Lens.lens (\Site' {tags} -> tags) (\s@Site' {} a -> s {tags = a} :: Site) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Site where
  parseJSON =
    Core.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "SiteId")
            Prelude.<*> (x Core..:? "SiteArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Site where
  hashWithSalt _salt Site' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` siteArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Site where
  rnf Site' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf siteArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags

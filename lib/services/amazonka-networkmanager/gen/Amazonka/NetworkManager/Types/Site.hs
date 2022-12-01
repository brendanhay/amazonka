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
-- Module      : Amazonka.NetworkManager.Types.Site
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Site where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.SiteState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a site.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the site.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the site.
    siteArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the site.
    state :: Prelude.Maybe SiteState,
    -- | The description of the site.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The location of the site.
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | The date and time that the site was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Site' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'site_globalNetworkId' - The ID of the global network.
--
-- 'tags', 'site_tags' - The tags for the site.
--
-- 'siteArn', 'site_siteArn' - The Amazon Resource Name (ARN) of the site.
--
-- 'state', 'site_state' - The state of the site.
--
-- 'description', 'site_description' - The description of the site.
--
-- 'siteId', 'site_siteId' - The ID of the site.
--
-- 'location', 'site_location' - The location of the site.
--
-- 'createdAt', 'site_createdAt' - The date and time that the site was created.
newSite ::
  Site
newSite =
  Site'
    { globalNetworkId = Prelude.Nothing,
      tags = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      siteId = Prelude.Nothing,
      location = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The ID of the global network.
site_globalNetworkId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_globalNetworkId = Lens.lens (\Site' {globalNetworkId} -> globalNetworkId) (\s@Site' {} a -> s {globalNetworkId = a} :: Site)

-- | The tags for the site.
site_tags :: Lens.Lens' Site (Prelude.Maybe [Tag])
site_tags = Lens.lens (\Site' {tags} -> tags) (\s@Site' {} a -> s {tags = a} :: Site) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the site.
site_siteArn :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteArn = Lens.lens (\Site' {siteArn} -> siteArn) (\s@Site' {} a -> s {siteArn = a} :: Site)

-- | The state of the site.
site_state :: Lens.Lens' Site (Prelude.Maybe SiteState)
site_state = Lens.lens (\Site' {state} -> state) (\s@Site' {} a -> s {state = a} :: Site)

-- | The description of the site.
site_description :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_description = Lens.lens (\Site' {description} -> description) (\s@Site' {} a -> s {description = a} :: Site)

-- | The ID of the site.
site_siteId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteId = Lens.lens (\Site' {siteId} -> siteId) (\s@Site' {} a -> s {siteId = a} :: Site)

-- | The location of the site.
site_location :: Lens.Lens' Site (Prelude.Maybe Location)
site_location = Lens.lens (\Site' {location} -> location) (\s@Site' {} a -> s {location = a} :: Site) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that the site was created.
site_createdAt :: Lens.Lens' Site (Prelude.Maybe Prelude.UTCTime)
site_createdAt = Lens.lens (\Site' {createdAt} -> createdAt) (\s@Site' {} a -> s {createdAt = a} :: Site) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Site where
  parseJSON =
    Core.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SiteArn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SiteId")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable Site where
  hashWithSalt _salt Site' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` siteArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Site where
  rnf Site' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf siteArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf createdAt

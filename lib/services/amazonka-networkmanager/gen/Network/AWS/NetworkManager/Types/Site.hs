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
-- Module      : Network.AWS.NetworkManager.Types.Site
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkManager.Types.Site where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkManager.Types.Location
import Network.AWS.NetworkManager.Types.SiteState
import Network.AWS.NetworkManager.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a site.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { -- | The state of the site.
    state :: Prelude.Maybe SiteState,
    -- | The location of the site.
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | The date and time that the site was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the site.
    siteArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the site.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the site.
    tags :: Prelude.Maybe [Tag]
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
-- 'state', 'site_state' - The state of the site.
--
-- 'location', 'site_location' - The location of the site.
--
-- 'createdAt', 'site_createdAt' - The date and time that the site was created.
--
-- 'globalNetworkId', 'site_globalNetworkId' - The ID of the global network.
--
-- 'siteId', 'site_siteId' - The ID of the site.
--
-- 'siteArn', 'site_siteArn' - The Amazon Resource Name (ARN) of the site.
--
-- 'description', 'site_description' - The description of the site.
--
-- 'tags', 'site_tags' - The tags for the site.
newSite ::
  Site
newSite =
  Site'
    { state = Prelude.Nothing,
      location = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      siteId = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the site.
site_state :: Lens.Lens' Site (Prelude.Maybe SiteState)
site_state = Lens.lens (\Site' {state} -> state) (\s@Site' {} a -> s {state = a} :: Site)

-- | The location of the site.
site_location :: Lens.Lens' Site (Prelude.Maybe Location)
site_location = Lens.lens (\Site' {location} -> location) (\s@Site' {} a -> s {location = a} :: Site) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that the site was created.
site_createdAt :: Lens.Lens' Site (Prelude.Maybe Prelude.UTCTime)
site_createdAt = Lens.lens (\Site' {createdAt} -> createdAt) (\s@Site' {} a -> s {createdAt = a} :: Site) Prelude.. Lens.mapping Core._Time

-- | The ID of the global network.
site_globalNetworkId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_globalNetworkId = Lens.lens (\Site' {globalNetworkId} -> globalNetworkId) (\s@Site' {} a -> s {globalNetworkId = a} :: Site)

-- | The ID of the site.
site_siteId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteId = Lens.lens (\Site' {siteId} -> siteId) (\s@Site' {} a -> s {siteId = a} :: Site)

-- | The Amazon Resource Name (ARN) of the site.
site_siteArn :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteArn = Lens.lens (\Site' {siteArn} -> siteArn) (\s@Site' {} a -> s {siteArn = a} :: Site)

-- | The description of the site.
site_description :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_description = Lens.lens (\Site' {description} -> description) (\s@Site' {} a -> s {description = a} :: Site)

-- | The tags for the site.
site_tags :: Lens.Lens' Site (Prelude.Maybe [Tag])
site_tags = Lens.lens (\Site' {tags} -> tags) (\s@Site' {} a -> s {tags = a} :: Site) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Site where
  parseJSON =
    Core.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "SiteId")
            Prelude.<*> (x Core..:? "SiteArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Site

instance Prelude.NFData Site

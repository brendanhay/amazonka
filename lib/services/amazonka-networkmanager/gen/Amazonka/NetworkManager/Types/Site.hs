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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Site where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.SiteState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a site.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { -- | The date and time that the site was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the site.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The location of the site.
    location :: Prelude.Maybe (Data.Sensitive Location),
    -- | The Amazon Resource Name (ARN) of the site.
    siteArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The state of the site.
    state :: Prelude.Maybe SiteState,
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
-- 'createdAt', 'site_createdAt' - The date and time that the site was created.
--
-- 'description', 'site_description' - The description of the site.
--
-- 'globalNetworkId', 'site_globalNetworkId' - The ID of the global network.
--
-- 'location', 'site_location' - The location of the site.
--
-- 'siteArn', 'site_siteArn' - The Amazon Resource Name (ARN) of the site.
--
-- 'siteId', 'site_siteId' - The ID of the site.
--
-- 'state', 'site_state' - The state of the site.
--
-- 'tags', 'site_tags' - The tags for the site.
newSite ::
  Site
newSite =
  Site'
    { createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      location = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      siteId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The date and time that the site was created.
site_createdAt :: Lens.Lens' Site (Prelude.Maybe Prelude.UTCTime)
site_createdAt = Lens.lens (\Site' {createdAt} -> createdAt) (\s@Site' {} a -> s {createdAt = a} :: Site) Prelude.. Lens.mapping Data._Time

-- | The description of the site.
site_description :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_description = Lens.lens (\Site' {description} -> description) (\s@Site' {} a -> s {description = a} :: Site)

-- | The ID of the global network.
site_globalNetworkId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_globalNetworkId = Lens.lens (\Site' {globalNetworkId} -> globalNetworkId) (\s@Site' {} a -> s {globalNetworkId = a} :: Site)

-- | The location of the site.
site_location :: Lens.Lens' Site (Prelude.Maybe Location)
site_location = Lens.lens (\Site' {location} -> location) (\s@Site' {} a -> s {location = a} :: Site) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the site.
site_siteArn :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteArn = Lens.lens (\Site' {siteArn} -> siteArn) (\s@Site' {} a -> s {siteArn = a} :: Site)

-- | The ID of the site.
site_siteId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteId = Lens.lens (\Site' {siteId} -> siteId) (\s@Site' {} a -> s {siteId = a} :: Site)

-- | The state of the site.
site_state :: Lens.Lens' Site (Prelude.Maybe SiteState)
site_state = Lens.lens (\Site' {state} -> state) (\s@Site' {} a -> s {state = a} :: Site)

-- | The tags for the site.
site_tags :: Lens.Lens' Site (Prelude.Maybe [Tag])
site_tags = Lens.lens (\Site' {tags} -> tags) (\s@Site' {} a -> s {tags = a} :: Site) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Site where
  parseJSON =
    Data.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "SiteArn")
            Prelude.<*> (x Data..:? "SiteId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Site where
  hashWithSalt _salt Site' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` siteArn
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Site where
  rnf Site' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf siteArn
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags

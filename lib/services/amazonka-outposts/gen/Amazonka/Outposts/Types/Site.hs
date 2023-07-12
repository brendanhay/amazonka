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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Site where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.RackPhysicalProperties
import qualified Amazonka.Prelude as Prelude

-- | Information about a site.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { accountId :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    -- | Notes about a site.
    notes :: Prelude.Maybe Prelude.Text,
    -- | City where the hardware is installed and powered on.
    operatingAddressCity :: Prelude.Maybe Prelude.Text,
    -- | The ISO-3166 two-letter country code where the hardware is installed and
    -- powered on.
    operatingAddressCountryCode :: Prelude.Maybe Prelude.Text,
    -- | State or region where the hardware is installed and powered on.
    operatingAddressStateOrRegion :: Prelude.Maybe Prelude.Text,
    -- | Information about the physical and logistical details for a rack at the
    -- site.
    rackPhysicalProperties :: Prelude.Maybe RackPhysicalProperties,
    siteArn :: Prelude.Maybe Prelude.Text,
    siteId :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'site_description' - Undocumented member.
--
-- 'name', 'site_name' - Undocumented member.
--
-- 'notes', 'site_notes' - Notes about a site.
--
-- 'operatingAddressCity', 'site_operatingAddressCity' - City where the hardware is installed and powered on.
--
-- 'operatingAddressCountryCode', 'site_operatingAddressCountryCode' - The ISO-3166 two-letter country code where the hardware is installed and
-- powered on.
--
-- 'operatingAddressStateOrRegion', 'site_operatingAddressStateOrRegion' - State or region where the hardware is installed and powered on.
--
-- 'rackPhysicalProperties', 'site_rackPhysicalProperties' - Information about the physical and logistical details for a rack at the
-- site.
--
-- 'siteArn', 'site_siteArn' - Undocumented member.
--
-- 'siteId', 'site_siteId' - Undocumented member.
--
-- 'tags', 'site_tags' - The site tags.
newSite ::
  Site
newSite =
  Site'
    { accountId = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      notes = Prelude.Nothing,
      operatingAddressCity = Prelude.Nothing,
      operatingAddressCountryCode = Prelude.Nothing,
      operatingAddressStateOrRegion = Prelude.Nothing,
      rackPhysicalProperties = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      siteId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
site_accountId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_accountId = Lens.lens (\Site' {accountId} -> accountId) (\s@Site' {} a -> s {accountId = a} :: Site)

-- | Undocumented member.
site_description :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_description = Lens.lens (\Site' {description} -> description) (\s@Site' {} a -> s {description = a} :: Site)

-- | Undocumented member.
site_name :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_name = Lens.lens (\Site' {name} -> name) (\s@Site' {} a -> s {name = a} :: Site)

-- | Notes about a site.
site_notes :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_notes = Lens.lens (\Site' {notes} -> notes) (\s@Site' {} a -> s {notes = a} :: Site)

-- | City where the hardware is installed and powered on.
site_operatingAddressCity :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_operatingAddressCity = Lens.lens (\Site' {operatingAddressCity} -> operatingAddressCity) (\s@Site' {} a -> s {operatingAddressCity = a} :: Site)

-- | The ISO-3166 two-letter country code where the hardware is installed and
-- powered on.
site_operatingAddressCountryCode :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_operatingAddressCountryCode = Lens.lens (\Site' {operatingAddressCountryCode} -> operatingAddressCountryCode) (\s@Site' {} a -> s {operatingAddressCountryCode = a} :: Site)

-- | State or region where the hardware is installed and powered on.
site_operatingAddressStateOrRegion :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_operatingAddressStateOrRegion = Lens.lens (\Site' {operatingAddressStateOrRegion} -> operatingAddressStateOrRegion) (\s@Site' {} a -> s {operatingAddressStateOrRegion = a} :: Site)

-- | Information about the physical and logistical details for a rack at the
-- site.
site_rackPhysicalProperties :: Lens.Lens' Site (Prelude.Maybe RackPhysicalProperties)
site_rackPhysicalProperties = Lens.lens (\Site' {rackPhysicalProperties} -> rackPhysicalProperties) (\s@Site' {} a -> s {rackPhysicalProperties = a} :: Site)

-- | Undocumented member.
site_siteArn :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteArn = Lens.lens (\Site' {siteArn} -> siteArn) (\s@Site' {} a -> s {siteArn = a} :: Site)

-- | Undocumented member.
site_siteId :: Lens.Lens' Site (Prelude.Maybe Prelude.Text)
site_siteId = Lens.lens (\Site' {siteId} -> siteId) (\s@Site' {} a -> s {siteId = a} :: Site)

-- | The site tags.
site_tags :: Lens.Lens' Site (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
site_tags = Lens.lens (\Site' {tags} -> tags) (\s@Site' {} a -> s {tags = a} :: Site) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Site where
  parseJSON =
    Data.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Notes")
            Prelude.<*> (x Data..:? "OperatingAddressCity")
            Prelude.<*> (x Data..:? "OperatingAddressCountryCode")
            Prelude.<*> (x Data..:? "OperatingAddressStateOrRegion")
            Prelude.<*> (x Data..:? "RackPhysicalProperties")
            Prelude.<*> (x Data..:? "SiteArn")
            Prelude.<*> (x Data..:? "SiteId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Site where
  hashWithSalt _salt Site' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` operatingAddressCity
      `Prelude.hashWithSalt` operatingAddressCountryCode
      `Prelude.hashWithSalt` operatingAddressStateOrRegion
      `Prelude.hashWithSalt` rackPhysicalProperties
      `Prelude.hashWithSalt` siteArn
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Site where
  rnf Site' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf operatingAddressCity
      `Prelude.seq` Prelude.rnf operatingAddressCountryCode
      `Prelude.seq` Prelude.rnf operatingAddressStateOrRegion
      `Prelude.seq` Prelude.rnf rackPhysicalProperties
      `Prelude.seq` Prelude.rnf siteArn
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf tags

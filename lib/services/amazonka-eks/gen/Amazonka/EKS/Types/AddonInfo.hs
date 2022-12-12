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
-- Module      : Amazonka.EKS.Types.AddonInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AddonVersionInfo
import Amazonka.EKS.Types.MarketplaceInformation
import qualified Amazonka.Prelude as Prelude

-- | Information about an add-on.
--
-- /See:/ 'newAddonInfo' smart constructor.
data AddonInfo = AddonInfo'
  { -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | An object representing information about available add-on versions and
    -- compatible Kubernetes versions.
    addonVersions :: Prelude.Maybe [AddonVersionInfo],
    -- | Information about the add-on from the Amazon Web Services Marketplace.
    marketplaceInformation :: Prelude.Maybe MarketplaceInformation,
    -- | The owner of the add-on.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The publisher of the add-on.
    publisher :: Prelude.Maybe Prelude.Text,
    -- | The type of the add-on.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonName', 'addonInfo_addonName' - The name of the add-on.
--
-- 'addonVersions', 'addonInfo_addonVersions' - An object representing information about available add-on versions and
-- compatible Kubernetes versions.
--
-- 'marketplaceInformation', 'addonInfo_marketplaceInformation' - Information about the add-on from the Amazon Web Services Marketplace.
--
-- 'owner', 'addonInfo_owner' - The owner of the add-on.
--
-- 'publisher', 'addonInfo_publisher' - The publisher of the add-on.
--
-- 'type'', 'addonInfo_type' - The type of the add-on.
newAddonInfo ::
  AddonInfo
newAddonInfo =
  AddonInfo'
    { addonName = Prelude.Nothing,
      addonVersions = Prelude.Nothing,
      marketplaceInformation = Prelude.Nothing,
      owner = Prelude.Nothing,
      publisher = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the add-on.
addonInfo_addonName :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_addonName = Lens.lens (\AddonInfo' {addonName} -> addonName) (\s@AddonInfo' {} a -> s {addonName = a} :: AddonInfo)

-- | An object representing information about available add-on versions and
-- compatible Kubernetes versions.
addonInfo_addonVersions :: Lens.Lens' AddonInfo (Prelude.Maybe [AddonVersionInfo])
addonInfo_addonVersions = Lens.lens (\AddonInfo' {addonVersions} -> addonVersions) (\s@AddonInfo' {} a -> s {addonVersions = a} :: AddonInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about the add-on from the Amazon Web Services Marketplace.
addonInfo_marketplaceInformation :: Lens.Lens' AddonInfo (Prelude.Maybe MarketplaceInformation)
addonInfo_marketplaceInformation = Lens.lens (\AddonInfo' {marketplaceInformation} -> marketplaceInformation) (\s@AddonInfo' {} a -> s {marketplaceInformation = a} :: AddonInfo)

-- | The owner of the add-on.
addonInfo_owner :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_owner = Lens.lens (\AddonInfo' {owner} -> owner) (\s@AddonInfo' {} a -> s {owner = a} :: AddonInfo)

-- | The publisher of the add-on.
addonInfo_publisher :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_publisher = Lens.lens (\AddonInfo' {publisher} -> publisher) (\s@AddonInfo' {} a -> s {publisher = a} :: AddonInfo)

-- | The type of the add-on.
addonInfo_type :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_type = Lens.lens (\AddonInfo' {type'} -> type') (\s@AddonInfo' {} a -> s {type' = a} :: AddonInfo)

instance Data.FromJSON AddonInfo where
  parseJSON =
    Data.withObject
      "AddonInfo"
      ( \x ->
          AddonInfo'
            Prelude.<$> (x Data..:? "addonName")
            Prelude.<*> (x Data..:? "addonVersions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "marketplaceInformation")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "publisher")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable AddonInfo where
  hashWithSalt _salt AddonInfo' {..} =
    _salt `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` addonVersions
      `Prelude.hashWithSalt` marketplaceInformation
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` publisher
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AddonInfo where
  rnf AddonInfo' {..} =
    Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf addonVersions
      `Prelude.seq` Prelude.rnf marketplaceInformation
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf publisher
      `Prelude.seq` Prelude.rnf type'

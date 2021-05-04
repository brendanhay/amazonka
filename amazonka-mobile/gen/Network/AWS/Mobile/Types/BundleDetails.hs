{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Mobile.Types.BundleDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.BundleDetails where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.Platform
import qualified Network.AWS.Prelude as Prelude

-- | The details of the bundle.
--
-- /See:/ 'newBundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { bundleId :: Prelude.Maybe Prelude.Text,
    title :: Prelude.Maybe Prelude.Text,
    iconUrl :: Prelude.Maybe Prelude.Text,
    version :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    availablePlatforms :: Prelude.Maybe [Platform]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BundleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'bundleDetails_bundleId' - Undocumented member.
--
-- 'title', 'bundleDetails_title' - Undocumented member.
--
-- 'iconUrl', 'bundleDetails_iconUrl' - Undocumented member.
--
-- 'version', 'bundleDetails_version' - Undocumented member.
--
-- 'description', 'bundleDetails_description' - Undocumented member.
--
-- 'availablePlatforms', 'bundleDetails_availablePlatforms' - Undocumented member.
newBundleDetails ::
  BundleDetails
newBundleDetails =
  BundleDetails'
    { bundleId = Prelude.Nothing,
      title = Prelude.Nothing,
      iconUrl = Prelude.Nothing,
      version = Prelude.Nothing,
      description = Prelude.Nothing,
      availablePlatforms = Prelude.Nothing
    }

-- | Undocumented member.
bundleDetails_bundleId :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_bundleId = Lens.lens (\BundleDetails' {bundleId} -> bundleId) (\s@BundleDetails' {} a -> s {bundleId = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_title :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_title = Lens.lens (\BundleDetails' {title} -> title) (\s@BundleDetails' {} a -> s {title = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_iconUrl :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_iconUrl = Lens.lens (\BundleDetails' {iconUrl} -> iconUrl) (\s@BundleDetails' {} a -> s {iconUrl = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_version :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_version = Lens.lens (\BundleDetails' {version} -> version) (\s@BundleDetails' {} a -> s {version = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_description :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_description = Lens.lens (\BundleDetails' {description} -> description) (\s@BundleDetails' {} a -> s {description = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_availablePlatforms :: Lens.Lens' BundleDetails (Prelude.Maybe [Platform])
bundleDetails_availablePlatforms = Lens.lens (\BundleDetails' {availablePlatforms} -> availablePlatforms) (\s@BundleDetails' {} a -> s {availablePlatforms = a} :: BundleDetails) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BundleDetails where
  parseJSON =
    Prelude.withObject
      "BundleDetails"
      ( \x ->
          BundleDetails'
            Prelude.<$> (x Prelude..:? "bundleId")
            Prelude.<*> (x Prelude..:? "title")
            Prelude.<*> (x Prelude..:? "iconUrl")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> ( x Prelude..:? "availablePlatforms"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BundleDetails

instance Prelude.NFData BundleDetails

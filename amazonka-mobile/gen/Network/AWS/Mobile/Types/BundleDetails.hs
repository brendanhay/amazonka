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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.Platform

-- | The details of the bundle.
--
-- /See:/ 'newBundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { bundleId :: Core.Maybe Core.Text,
    title :: Core.Maybe Core.Text,
    iconUrl :: Core.Maybe Core.Text,
    version :: Core.Maybe Core.Text,
    description :: Core.Maybe Core.Text,
    availablePlatforms :: Core.Maybe [Platform]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { bundleId = Core.Nothing,
      title = Core.Nothing,
      iconUrl = Core.Nothing,
      version = Core.Nothing,
      description = Core.Nothing,
      availablePlatforms = Core.Nothing
    }

-- | Undocumented member.
bundleDetails_bundleId :: Lens.Lens' BundleDetails (Core.Maybe Core.Text)
bundleDetails_bundleId = Lens.lens (\BundleDetails' {bundleId} -> bundleId) (\s@BundleDetails' {} a -> s {bundleId = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_title :: Lens.Lens' BundleDetails (Core.Maybe Core.Text)
bundleDetails_title = Lens.lens (\BundleDetails' {title} -> title) (\s@BundleDetails' {} a -> s {title = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_iconUrl :: Lens.Lens' BundleDetails (Core.Maybe Core.Text)
bundleDetails_iconUrl = Lens.lens (\BundleDetails' {iconUrl} -> iconUrl) (\s@BundleDetails' {} a -> s {iconUrl = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_version :: Lens.Lens' BundleDetails (Core.Maybe Core.Text)
bundleDetails_version = Lens.lens (\BundleDetails' {version} -> version) (\s@BundleDetails' {} a -> s {version = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_description :: Lens.Lens' BundleDetails (Core.Maybe Core.Text)
bundleDetails_description = Lens.lens (\BundleDetails' {description} -> description) (\s@BundleDetails' {} a -> s {description = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_availablePlatforms :: Lens.Lens' BundleDetails (Core.Maybe [Platform])
bundleDetails_availablePlatforms = Lens.lens (\BundleDetails' {availablePlatforms} -> availablePlatforms) (\s@BundleDetails' {} a -> s {availablePlatforms = a} :: BundleDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BundleDetails where
  parseJSON =
    Core.withObject
      "BundleDetails"
      ( \x ->
          BundleDetails'
            Core.<$> (x Core..:? "bundleId")
            Core.<*> (x Core..:? "title")
            Core.<*> (x Core..:? "iconUrl")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "description")
            Core.<*> ( x Core..:? "availablePlatforms"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable BundleDetails

instance Core.NFData BundleDetails

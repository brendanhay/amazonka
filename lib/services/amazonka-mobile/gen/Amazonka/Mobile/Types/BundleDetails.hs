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
-- Module      : Amazonka.Mobile.Types.BundleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types.BundleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | The details of the bundle.
--
-- /See:/ 'newBundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { availablePlatforms :: Prelude.Maybe [Platform],
    bundleId :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    iconUrl :: Prelude.Maybe Prelude.Text,
    title :: Prelude.Maybe Prelude.Text,
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BundleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availablePlatforms', 'bundleDetails_availablePlatforms' - Undocumented member.
--
-- 'bundleId', 'bundleDetails_bundleId' - Undocumented member.
--
-- 'description', 'bundleDetails_description' - Undocumented member.
--
-- 'iconUrl', 'bundleDetails_iconUrl' - Undocumented member.
--
-- 'title', 'bundleDetails_title' - Undocumented member.
--
-- 'version', 'bundleDetails_version' - Undocumented member.
newBundleDetails ::
  BundleDetails
newBundleDetails =
  BundleDetails'
    { availablePlatforms =
        Prelude.Nothing,
      bundleId = Prelude.Nothing,
      description = Prelude.Nothing,
      iconUrl = Prelude.Nothing,
      title = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Undocumented member.
bundleDetails_availablePlatforms :: Lens.Lens' BundleDetails (Prelude.Maybe [Platform])
bundleDetails_availablePlatforms = Lens.lens (\BundleDetails' {availablePlatforms} -> availablePlatforms) (\s@BundleDetails' {} a -> s {availablePlatforms = a} :: BundleDetails) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
bundleDetails_bundleId :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_bundleId = Lens.lens (\BundleDetails' {bundleId} -> bundleId) (\s@BundleDetails' {} a -> s {bundleId = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_description :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_description = Lens.lens (\BundleDetails' {description} -> description) (\s@BundleDetails' {} a -> s {description = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_iconUrl :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_iconUrl = Lens.lens (\BundleDetails' {iconUrl} -> iconUrl) (\s@BundleDetails' {} a -> s {iconUrl = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_title :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_title = Lens.lens (\BundleDetails' {title} -> title) (\s@BundleDetails' {} a -> s {title = a} :: BundleDetails)

-- | Undocumented member.
bundleDetails_version :: Lens.Lens' BundleDetails (Prelude.Maybe Prelude.Text)
bundleDetails_version = Lens.lens (\BundleDetails' {version} -> version) (\s@BundleDetails' {} a -> s {version = a} :: BundleDetails)

instance Data.FromJSON BundleDetails where
  parseJSON =
    Data.withObject
      "BundleDetails"
      ( \x ->
          BundleDetails'
            Prelude.<$> ( x
                            Data..:? "availablePlatforms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "bundleId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "iconUrl")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable BundleDetails where
  hashWithSalt _salt BundleDetails' {..} =
    _salt
      `Prelude.hashWithSalt` availablePlatforms
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iconUrl
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` version

instance Prelude.NFData BundleDetails where
  rnf BundleDetails' {..} =
    Prelude.rnf availablePlatforms
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf iconUrl
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf version

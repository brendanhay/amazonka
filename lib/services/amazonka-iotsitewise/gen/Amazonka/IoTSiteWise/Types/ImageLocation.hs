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
-- Module      : Amazonka.IoTSiteWise.Types.ImageLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ImageLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an image that is uploaded to IoT SiteWise and available at a
-- URL.
--
-- /See:/ 'newImageLocation' smart constructor.
data ImageLocation = ImageLocation'
  { -- | The ID of the image.
    id :: Prelude.Text,
    -- | The URL where the image is available. The URL is valid for 15 minutes so
    -- that you can view and download the image
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'imageLocation_id' - The ID of the image.
--
-- 'url', 'imageLocation_url' - The URL where the image is available. The URL is valid for 15 minutes so
-- that you can view and download the image
newImageLocation ::
  -- | 'id'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  ImageLocation
newImageLocation pId_ pUrl_ =
  ImageLocation' {id = pId_, url = pUrl_}

-- | The ID of the image.
imageLocation_id :: Lens.Lens' ImageLocation Prelude.Text
imageLocation_id = Lens.lens (\ImageLocation' {id} -> id) (\s@ImageLocation' {} a -> s {id = a} :: ImageLocation)

-- | The URL where the image is available. The URL is valid for 15 minutes so
-- that you can view and download the image
imageLocation_url :: Lens.Lens' ImageLocation Prelude.Text
imageLocation_url = Lens.lens (\ImageLocation' {url} -> url) (\s@ImageLocation' {} a -> s {url = a} :: ImageLocation)

instance Core.FromJSON ImageLocation where
  parseJSON =
    Core.withObject
      "ImageLocation"
      ( \x ->
          ImageLocation'
            Prelude.<$> (x Core..: "id") Prelude.<*> (x Core..: "url")
      )

instance Prelude.Hashable ImageLocation where
  hashWithSalt _salt ImageLocation' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` url

instance Prelude.NFData ImageLocation where
  rnf ImageLocation' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf url

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
-- Module      : Amazonka.EMRServerless.Types.ImageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.ImageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The applied image configuration.
--
-- /See:/ 'newImageConfiguration' smart constructor.
data ImageConfiguration = ImageConfiguration'
  { -- | The SHA256 digest of the image URI. This indicates which specific image
    -- the application is configured for. The image digest doesn\'t exist until
    -- an application has started.
    resolvedImageDigest :: Prelude.Maybe Prelude.Text,
    -- | The image URI.
    imageUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolvedImageDigest', 'imageConfiguration_resolvedImageDigest' - The SHA256 digest of the image URI. This indicates which specific image
-- the application is configured for. The image digest doesn\'t exist until
-- an application has started.
--
-- 'imageUri', 'imageConfiguration_imageUri' - The image URI.
newImageConfiguration ::
  -- | 'imageUri'
  Prelude.Text ->
  ImageConfiguration
newImageConfiguration pImageUri_ =
  ImageConfiguration'
    { resolvedImageDigest =
        Prelude.Nothing,
      imageUri = pImageUri_
    }

-- | The SHA256 digest of the image URI. This indicates which specific image
-- the application is configured for. The image digest doesn\'t exist until
-- an application has started.
imageConfiguration_resolvedImageDigest :: Lens.Lens' ImageConfiguration (Prelude.Maybe Prelude.Text)
imageConfiguration_resolvedImageDigest = Lens.lens (\ImageConfiguration' {resolvedImageDigest} -> resolvedImageDigest) (\s@ImageConfiguration' {} a -> s {resolvedImageDigest = a} :: ImageConfiguration)

-- | The image URI.
imageConfiguration_imageUri :: Lens.Lens' ImageConfiguration Prelude.Text
imageConfiguration_imageUri = Lens.lens (\ImageConfiguration' {imageUri} -> imageUri) (\s@ImageConfiguration' {} a -> s {imageUri = a} :: ImageConfiguration)

instance Data.FromJSON ImageConfiguration where
  parseJSON =
    Data.withObject
      "ImageConfiguration"
      ( \x ->
          ImageConfiguration'
            Prelude.<$> (x Data..:? "resolvedImageDigest")
            Prelude.<*> (x Data..: "imageUri")
      )

instance Prelude.Hashable ImageConfiguration where
  hashWithSalt _salt ImageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` resolvedImageDigest
      `Prelude.hashWithSalt` imageUri

instance Prelude.NFData ImageConfiguration where
  rnf ImageConfiguration' {..} =
    Prelude.rnf resolvedImageDigest
      `Prelude.seq` Prelude.rnf imageUri

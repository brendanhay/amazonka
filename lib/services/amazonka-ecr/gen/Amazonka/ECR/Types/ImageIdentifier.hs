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
-- Module      : Amazonka.ECR.Types.ImageIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object with identifying information for an image in an Amazon ECR
-- repository.
--
-- /See:/ 'newImageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { -- | The tag used for the image.
    imageTag :: Prelude.Maybe Prelude.Text,
    -- | The @sha256@ digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageTag', 'imageIdentifier_imageTag' - The tag used for the image.
--
-- 'imageDigest', 'imageIdentifier_imageDigest' - The @sha256@ digest of the image manifest.
newImageIdentifier ::
  ImageIdentifier
newImageIdentifier =
  ImageIdentifier'
    { imageTag = Prelude.Nothing,
      imageDigest = Prelude.Nothing
    }

-- | The tag used for the image.
imageIdentifier_imageTag :: Lens.Lens' ImageIdentifier (Prelude.Maybe Prelude.Text)
imageIdentifier_imageTag = Lens.lens (\ImageIdentifier' {imageTag} -> imageTag) (\s@ImageIdentifier' {} a -> s {imageTag = a} :: ImageIdentifier)

-- | The @sha256@ digest of the image manifest.
imageIdentifier_imageDigest :: Lens.Lens' ImageIdentifier (Prelude.Maybe Prelude.Text)
imageIdentifier_imageDigest = Lens.lens (\ImageIdentifier' {imageDigest} -> imageDigest) (\s@ImageIdentifier' {} a -> s {imageDigest = a} :: ImageIdentifier)

instance Core.FromJSON ImageIdentifier where
  parseJSON =
    Core.withObject
      "ImageIdentifier"
      ( \x ->
          ImageIdentifier'
            Prelude.<$> (x Core..:? "imageTag")
            Prelude.<*> (x Core..:? "imageDigest")
      )

instance Prelude.Hashable ImageIdentifier where
  hashWithSalt _salt ImageIdentifier' {..} =
    _salt `Prelude.hashWithSalt` imageTag
      `Prelude.hashWithSalt` imageDigest

instance Prelude.NFData ImageIdentifier where
  rnf ImageIdentifier' {..} =
    Prelude.rnf imageTag
      `Prelude.seq` Prelude.rnf imageDigest

instance Core.ToJSON ImageIdentifier where
  toJSON ImageIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("imageTag" Core..=) Prelude.<$> imageTag,
            ("imageDigest" Core..=) Prelude.<$> imageDigest
          ]
      )

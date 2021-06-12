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
-- Module      : Network.AWS.ECR.Types.ImageIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageIdentifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object with identifying information for an Amazon ECR image.
--
-- /See:/ 'newImageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { -- | The @sha256@ digest of the image manifest.
    imageDigest :: Core.Maybe Core.Text,
    -- | The tag used for the image.
    imageTag :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'imageIdentifier_imageDigest' - The @sha256@ digest of the image manifest.
--
-- 'imageTag', 'imageIdentifier_imageTag' - The tag used for the image.
newImageIdentifier ::
  ImageIdentifier
newImageIdentifier =
  ImageIdentifier'
    { imageDigest = Core.Nothing,
      imageTag = Core.Nothing
    }

-- | The @sha256@ digest of the image manifest.
imageIdentifier_imageDigest :: Lens.Lens' ImageIdentifier (Core.Maybe Core.Text)
imageIdentifier_imageDigest = Lens.lens (\ImageIdentifier' {imageDigest} -> imageDigest) (\s@ImageIdentifier' {} a -> s {imageDigest = a} :: ImageIdentifier)

-- | The tag used for the image.
imageIdentifier_imageTag :: Lens.Lens' ImageIdentifier (Core.Maybe Core.Text)
imageIdentifier_imageTag = Lens.lens (\ImageIdentifier' {imageTag} -> imageTag) (\s@ImageIdentifier' {} a -> s {imageTag = a} :: ImageIdentifier)

instance Core.FromJSON ImageIdentifier where
  parseJSON =
    Core.withObject
      "ImageIdentifier"
      ( \x ->
          ImageIdentifier'
            Core.<$> (x Core..:? "imageDigest")
            Core.<*> (x Core..:? "imageTag")
      )

instance Core.Hashable ImageIdentifier

instance Core.NFData ImageIdentifier

instance Core.ToJSON ImageIdentifier where
  toJSON ImageIdentifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("imageDigest" Core..=) Core.<$> imageDigest,
            ("imageTag" Core..=) Core.<$> imageTag
          ]
      )

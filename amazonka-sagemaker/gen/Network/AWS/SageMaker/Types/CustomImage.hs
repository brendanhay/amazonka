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
-- Module      : Network.AWS.SageMaker.Types.CustomImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CustomImage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A custom SageMaker image. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image>.
--
-- /See:/ 'newCustomImage' smart constructor.
data CustomImage = CustomImage'
  { -- | The version number of the CustomImage.
    imageVersionNumber :: Core.Maybe Core.Natural,
    -- | The name of the CustomImage. Must be unique to your account.
    imageName :: Core.Text,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersionNumber', 'customImage_imageVersionNumber' - The version number of the CustomImage.
--
-- 'imageName', 'customImage_imageName' - The name of the CustomImage. Must be unique to your account.
--
-- 'appImageConfigName', 'customImage_appImageConfigName' - The name of the AppImageConfig.
newCustomImage ::
  -- | 'imageName'
  Core.Text ->
  -- | 'appImageConfigName'
  Core.Text ->
  CustomImage
newCustomImage pImageName_ pAppImageConfigName_ =
  CustomImage'
    { imageVersionNumber = Core.Nothing,
      imageName = pImageName_,
      appImageConfigName = pAppImageConfigName_
    }

-- | The version number of the CustomImage.
customImage_imageVersionNumber :: Lens.Lens' CustomImage (Core.Maybe Core.Natural)
customImage_imageVersionNumber = Lens.lens (\CustomImage' {imageVersionNumber} -> imageVersionNumber) (\s@CustomImage' {} a -> s {imageVersionNumber = a} :: CustomImage)

-- | The name of the CustomImage. Must be unique to your account.
customImage_imageName :: Lens.Lens' CustomImage Core.Text
customImage_imageName = Lens.lens (\CustomImage' {imageName} -> imageName) (\s@CustomImage' {} a -> s {imageName = a} :: CustomImage)

-- | The name of the AppImageConfig.
customImage_appImageConfigName :: Lens.Lens' CustomImage Core.Text
customImage_appImageConfigName = Lens.lens (\CustomImage' {appImageConfigName} -> appImageConfigName) (\s@CustomImage' {} a -> s {appImageConfigName = a} :: CustomImage)

instance Core.FromJSON CustomImage where
  parseJSON =
    Core.withObject
      "CustomImage"
      ( \x ->
          CustomImage'
            Core.<$> (x Core..:? "ImageVersionNumber")
            Core.<*> (x Core..: "ImageName")
            Core.<*> (x Core..: "AppImageConfigName")
      )

instance Core.Hashable CustomImage

instance Core.NFData CustomImage

instance Core.ToJSON CustomImage where
  toJSON CustomImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageVersionNumber" Core..=)
              Core.<$> imageVersionNumber,
            Core.Just ("ImageName" Core..= imageName),
            Core.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

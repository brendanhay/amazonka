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
-- Module      : Network.AWS.SageMaker.Types.CustomImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CustomImage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A custom SageMaker image. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image>.
--
-- /See:/ 'newCustomImage' smart constructor.
data CustomImage = CustomImage'
  { -- | The version number of the CustomImage.
    imageVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The name of the CustomImage. Must be unique to your account.
    imageName :: Prelude.Text,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'appImageConfigName'
  Prelude.Text ->
  CustomImage
newCustomImage pImageName_ pAppImageConfigName_ =
  CustomImage'
    { imageVersionNumber = Prelude.Nothing,
      imageName = pImageName_,
      appImageConfigName = pAppImageConfigName_
    }

-- | The version number of the CustomImage.
customImage_imageVersionNumber :: Lens.Lens' CustomImage (Prelude.Maybe Prelude.Natural)
customImage_imageVersionNumber = Lens.lens (\CustomImage' {imageVersionNumber} -> imageVersionNumber) (\s@CustomImage' {} a -> s {imageVersionNumber = a} :: CustomImage)

-- | The name of the CustomImage. Must be unique to your account.
customImage_imageName :: Lens.Lens' CustomImage Prelude.Text
customImage_imageName = Lens.lens (\CustomImage' {imageName} -> imageName) (\s@CustomImage' {} a -> s {imageName = a} :: CustomImage)

-- | The name of the AppImageConfig.
customImage_appImageConfigName :: Lens.Lens' CustomImage Prelude.Text
customImage_appImageConfigName = Lens.lens (\CustomImage' {appImageConfigName} -> appImageConfigName) (\s@CustomImage' {} a -> s {appImageConfigName = a} :: CustomImage)

instance Prelude.FromJSON CustomImage where
  parseJSON =
    Prelude.withObject
      "CustomImage"
      ( \x ->
          CustomImage'
            Prelude.<$> (x Prelude..:? "ImageVersionNumber")
            Prelude.<*> (x Prelude..: "ImageName")
            Prelude.<*> (x Prelude..: "AppImageConfigName")
      )

instance Prelude.Hashable CustomImage

instance Prelude.NFData CustomImage

instance Prelude.ToJSON CustomImage where
  toJSON CustomImage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ImageVersionNumber" Prelude..=)
              Prelude.<$> imageVersionNumber,
            Prelude.Just ("ImageName" Prelude..= imageName),
            Prelude.Just
              ( "AppImageConfigName"
                  Prelude..= appImageConfigName
              )
          ]
      )

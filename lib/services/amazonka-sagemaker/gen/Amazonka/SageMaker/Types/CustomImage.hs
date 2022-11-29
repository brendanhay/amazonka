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
-- Module      : Amazonka.SageMaker.Types.CustomImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CustomImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON CustomImage where
  parseJSON =
    Core.withObject
      "CustomImage"
      ( \x ->
          CustomImage'
            Prelude.<$> (x Core..:? "ImageVersionNumber")
            Prelude.<*> (x Core..: "ImageName")
            Prelude.<*> (x Core..: "AppImageConfigName")
      )

instance Prelude.Hashable CustomImage where
  hashWithSalt _salt CustomImage' {..} =
    _salt `Prelude.hashWithSalt` imageVersionNumber
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` appImageConfigName

instance Prelude.NFData CustomImage where
  rnf CustomImage' {..} =
    Prelude.rnf imageVersionNumber
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf appImageConfigName

instance Core.ToJSON CustomImage where
  toJSON CustomImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ImageVersionNumber" Core..=)
              Prelude.<$> imageVersionNumber,
            Prelude.Just ("ImageName" Core..= imageName),
            Prelude.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

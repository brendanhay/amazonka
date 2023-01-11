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
-- Module      : Amazonka.AppRunner.Types.ImageRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ImageRepository where

import Amazonka.AppRunner.Types.ImageConfiguration
import Amazonka.AppRunner.Types.ImageRepositoryType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a source image repository.
--
-- /See:/ 'newImageRepository' smart constructor.
data ImageRepository = ImageRepository'
  { -- | Configuration for running the identified image.
    imageConfiguration :: Prelude.Maybe ImageConfiguration,
    -- | The identifier of an image.
    --
    -- For an image in Amazon Elastic Container Registry (Amazon ECR), this is
    -- an image name. For the image name format, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an image>
    -- in the /Amazon ECR User Guide/.
    imageIdentifier :: Prelude.Text,
    -- | The type of the image repository. This reflects the repository provider
    -- and whether the repository is private or public.
    imageRepositoryType :: ImageRepositoryType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfiguration', 'imageRepository_imageConfiguration' - Configuration for running the identified image.
--
-- 'imageIdentifier', 'imageRepository_imageIdentifier' - The identifier of an image.
--
-- For an image in Amazon Elastic Container Registry (Amazon ECR), this is
-- an image name. For the image name format, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an image>
-- in the /Amazon ECR User Guide/.
--
-- 'imageRepositoryType', 'imageRepository_imageRepositoryType' - The type of the image repository. This reflects the repository provider
-- and whether the repository is private or public.
newImageRepository ::
  -- | 'imageIdentifier'
  Prelude.Text ->
  -- | 'imageRepositoryType'
  ImageRepositoryType ->
  ImageRepository
newImageRepository
  pImageIdentifier_
  pImageRepositoryType_ =
    ImageRepository'
      { imageConfiguration =
          Prelude.Nothing,
        imageIdentifier = pImageIdentifier_,
        imageRepositoryType = pImageRepositoryType_
      }

-- | Configuration for running the identified image.
imageRepository_imageConfiguration :: Lens.Lens' ImageRepository (Prelude.Maybe ImageConfiguration)
imageRepository_imageConfiguration = Lens.lens (\ImageRepository' {imageConfiguration} -> imageConfiguration) (\s@ImageRepository' {} a -> s {imageConfiguration = a} :: ImageRepository)

-- | The identifier of an image.
--
-- For an image in Amazon Elastic Container Registry (Amazon ECR), this is
-- an image name. For the image name format, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an image>
-- in the /Amazon ECR User Guide/.
imageRepository_imageIdentifier :: Lens.Lens' ImageRepository Prelude.Text
imageRepository_imageIdentifier = Lens.lens (\ImageRepository' {imageIdentifier} -> imageIdentifier) (\s@ImageRepository' {} a -> s {imageIdentifier = a} :: ImageRepository)

-- | The type of the image repository. This reflects the repository provider
-- and whether the repository is private or public.
imageRepository_imageRepositoryType :: Lens.Lens' ImageRepository ImageRepositoryType
imageRepository_imageRepositoryType = Lens.lens (\ImageRepository' {imageRepositoryType} -> imageRepositoryType) (\s@ImageRepository' {} a -> s {imageRepositoryType = a} :: ImageRepository)

instance Data.FromJSON ImageRepository where
  parseJSON =
    Data.withObject
      "ImageRepository"
      ( \x ->
          ImageRepository'
            Prelude.<$> (x Data..:? "ImageConfiguration")
            Prelude.<*> (x Data..: "ImageIdentifier")
            Prelude.<*> (x Data..: "ImageRepositoryType")
      )

instance Prelude.Hashable ImageRepository where
  hashWithSalt _salt ImageRepository' {..} =
    _salt `Prelude.hashWithSalt` imageConfiguration
      `Prelude.hashWithSalt` imageIdentifier
      `Prelude.hashWithSalt` imageRepositoryType

instance Prelude.NFData ImageRepository where
  rnf ImageRepository' {..} =
    Prelude.rnf imageConfiguration
      `Prelude.seq` Prelude.rnf imageIdentifier
      `Prelude.seq` Prelude.rnf imageRepositoryType

instance Data.ToJSON ImageRepository where
  toJSON ImageRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImageConfiguration" Data..=)
              Prelude.<$> imageConfiguration,
            Prelude.Just
              ("ImageIdentifier" Data..= imageIdentifier),
            Prelude.Just
              ("ImageRepositoryType" Data..= imageRepositoryType)
          ]
      )

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
-- Module      : Amazonka.ECRPublic.Types.ImageTagDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.ImageTagDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types.ReferencedImageDetail
import qualified Amazonka.Prelude as Prelude

-- | An object representing the image tag details for an image.
--
-- /See:/ 'newImageTagDetail' smart constructor.
data ImageTagDetail = ImageTagDetail'
  { -- | The time stamp indicating when the image tag was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An object that describes the details of an image.
    imageDetail :: Prelude.Maybe ReferencedImageDetail,
    -- | The tag associated with the image.
    imageTag :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageTagDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'imageTagDetail_createdAt' - The time stamp indicating when the image tag was created.
--
-- 'imageDetail', 'imageTagDetail_imageDetail' - An object that describes the details of an image.
--
-- 'imageTag', 'imageTagDetail_imageTag' - The tag associated with the image.
newImageTagDetail ::
  ImageTagDetail
newImageTagDetail =
  ImageTagDetail'
    { createdAt = Prelude.Nothing,
      imageDetail = Prelude.Nothing,
      imageTag = Prelude.Nothing
    }

-- | The time stamp indicating when the image tag was created.
imageTagDetail_createdAt :: Lens.Lens' ImageTagDetail (Prelude.Maybe Prelude.UTCTime)
imageTagDetail_createdAt = Lens.lens (\ImageTagDetail' {createdAt} -> createdAt) (\s@ImageTagDetail' {} a -> s {createdAt = a} :: ImageTagDetail) Prelude.. Lens.mapping Data._Time

-- | An object that describes the details of an image.
imageTagDetail_imageDetail :: Lens.Lens' ImageTagDetail (Prelude.Maybe ReferencedImageDetail)
imageTagDetail_imageDetail = Lens.lens (\ImageTagDetail' {imageDetail} -> imageDetail) (\s@ImageTagDetail' {} a -> s {imageDetail = a} :: ImageTagDetail)

-- | The tag associated with the image.
imageTagDetail_imageTag :: Lens.Lens' ImageTagDetail (Prelude.Maybe Prelude.Text)
imageTagDetail_imageTag = Lens.lens (\ImageTagDetail' {imageTag} -> imageTag) (\s@ImageTagDetail' {} a -> s {imageTag = a} :: ImageTagDetail)

instance Data.FromJSON ImageTagDetail where
  parseJSON =
    Data.withObject
      "ImageTagDetail"
      ( \x ->
          ImageTagDetail'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "imageDetail")
            Prelude.<*> (x Data..:? "imageTag")
      )

instance Prelude.Hashable ImageTagDetail where
  hashWithSalt _salt ImageTagDetail' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` imageDetail
      `Prelude.hashWithSalt` imageTag

instance Prelude.NFData ImageTagDetail where
  rnf ImageTagDetail' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf imageDetail
      `Prelude.seq` Prelude.rnf imageTag

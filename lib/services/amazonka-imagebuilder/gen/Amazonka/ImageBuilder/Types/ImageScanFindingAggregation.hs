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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanFindingAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanFindingAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.AccountAggregation
import Amazonka.ImageBuilder.Types.ImageAggregation
import Amazonka.ImageBuilder.Types.ImagePipelineAggregation
import Amazonka.ImageBuilder.Types.VulnerabilityIdAggregation
import qualified Amazonka.Prelude as Prelude

-- | This returns exactly one type of aggregation, based on the filter that
-- Image Builder applies in its API action.
--
-- /See:/ 'newImageScanFindingAggregation' smart constructor.
data ImageScanFindingAggregation = ImageScanFindingAggregation'
  { -- | Returns an object that contains severity counts based on an account ID.
    accountAggregation :: Prelude.Maybe AccountAggregation,
    -- | Returns an object that contains severity counts based on the Amazon
    -- Resource Name (ARN) for a specific image.
    imageAggregation :: Prelude.Maybe ImageAggregation,
    -- | Returns an object that contains severity counts based on an image
    -- pipeline ARN.
    imagePipelineAggregation :: Prelude.Maybe ImagePipelineAggregation,
    -- | Returns an object that contains severity counts based on vulnerability
    -- ID.
    vulnerabilityIdAggregation :: Prelude.Maybe VulnerabilityIdAggregation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFindingAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAggregation', 'imageScanFindingAggregation_accountAggregation' - Returns an object that contains severity counts based on an account ID.
--
-- 'imageAggregation', 'imageScanFindingAggregation_imageAggregation' - Returns an object that contains severity counts based on the Amazon
-- Resource Name (ARN) for a specific image.
--
-- 'imagePipelineAggregation', 'imageScanFindingAggregation_imagePipelineAggregation' - Returns an object that contains severity counts based on an image
-- pipeline ARN.
--
-- 'vulnerabilityIdAggregation', 'imageScanFindingAggregation_vulnerabilityIdAggregation' - Returns an object that contains severity counts based on vulnerability
-- ID.
newImageScanFindingAggregation ::
  ImageScanFindingAggregation
newImageScanFindingAggregation =
  ImageScanFindingAggregation'
    { accountAggregation =
        Prelude.Nothing,
      imageAggregation = Prelude.Nothing,
      imagePipelineAggregation = Prelude.Nothing,
      vulnerabilityIdAggregation = Prelude.Nothing
    }

-- | Returns an object that contains severity counts based on an account ID.
imageScanFindingAggregation_accountAggregation :: Lens.Lens' ImageScanFindingAggregation (Prelude.Maybe AccountAggregation)
imageScanFindingAggregation_accountAggregation = Lens.lens (\ImageScanFindingAggregation' {accountAggregation} -> accountAggregation) (\s@ImageScanFindingAggregation' {} a -> s {accountAggregation = a} :: ImageScanFindingAggregation)

-- | Returns an object that contains severity counts based on the Amazon
-- Resource Name (ARN) for a specific image.
imageScanFindingAggregation_imageAggregation :: Lens.Lens' ImageScanFindingAggregation (Prelude.Maybe ImageAggregation)
imageScanFindingAggregation_imageAggregation = Lens.lens (\ImageScanFindingAggregation' {imageAggregation} -> imageAggregation) (\s@ImageScanFindingAggregation' {} a -> s {imageAggregation = a} :: ImageScanFindingAggregation)

-- | Returns an object that contains severity counts based on an image
-- pipeline ARN.
imageScanFindingAggregation_imagePipelineAggregation :: Lens.Lens' ImageScanFindingAggregation (Prelude.Maybe ImagePipelineAggregation)
imageScanFindingAggregation_imagePipelineAggregation = Lens.lens (\ImageScanFindingAggregation' {imagePipelineAggregation} -> imagePipelineAggregation) (\s@ImageScanFindingAggregation' {} a -> s {imagePipelineAggregation = a} :: ImageScanFindingAggregation)

-- | Returns an object that contains severity counts based on vulnerability
-- ID.
imageScanFindingAggregation_vulnerabilityIdAggregation :: Lens.Lens' ImageScanFindingAggregation (Prelude.Maybe VulnerabilityIdAggregation)
imageScanFindingAggregation_vulnerabilityIdAggregation = Lens.lens (\ImageScanFindingAggregation' {vulnerabilityIdAggregation} -> vulnerabilityIdAggregation) (\s@ImageScanFindingAggregation' {} a -> s {vulnerabilityIdAggregation = a} :: ImageScanFindingAggregation)

instance Data.FromJSON ImageScanFindingAggregation where
  parseJSON =
    Data.withObject
      "ImageScanFindingAggregation"
      ( \x ->
          ImageScanFindingAggregation'
            Prelude.<$> (x Data..:? "accountAggregation")
            Prelude.<*> (x Data..:? "imageAggregation")
            Prelude.<*> (x Data..:? "imagePipelineAggregation")
            Prelude.<*> (x Data..:? "vulnerabilityIdAggregation")
      )

instance Prelude.Hashable ImageScanFindingAggregation where
  hashWithSalt _salt ImageScanFindingAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` accountAggregation
      `Prelude.hashWithSalt` imageAggregation
      `Prelude.hashWithSalt` imagePipelineAggregation
      `Prelude.hashWithSalt` vulnerabilityIdAggregation

instance Prelude.NFData ImageScanFindingAggregation where
  rnf ImageScanFindingAggregation' {..} =
    Prelude.rnf accountAggregation
      `Prelude.seq` Prelude.rnf imageAggregation
      `Prelude.seq` Prelude.rnf imagePipelineAggregation
      `Prelude.seq` Prelude.rnf vulnerabilityIdAggregation

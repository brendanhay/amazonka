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
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionArn :: Core.Maybe Core.Text,
    -- | The SHA-256 hash of the layer archive.
    codeSha256 :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobArn :: Core.Maybe Core.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Core.Maybe Core.Integer,
    -- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
    location :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LayerVersionContentOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingProfileVersionArn', 'layerVersionContentOutput_signingProfileVersionArn' - The Amazon Resource Name (ARN) for a signing profile version.
--
-- 'codeSha256', 'layerVersionContentOutput_codeSha256' - The SHA-256 hash of the layer archive.
--
-- 'signingJobArn', 'layerVersionContentOutput_signingJobArn' - The Amazon Resource Name (ARN) of a signing job.
--
-- 'codeSize', 'layerVersionContentOutput_codeSize' - The size of the layer archive in bytes.
--
-- 'location', 'layerVersionContentOutput_location' - A link to the layer archive in Amazon S3 that is valid for 10 minutes.
newLayerVersionContentOutput ::
  LayerVersionContentOutput
newLayerVersionContentOutput =
  LayerVersionContentOutput'
    { signingProfileVersionArn =
        Core.Nothing,
      codeSha256 = Core.Nothing,
      signingJobArn = Core.Nothing,
      codeSize = Core.Nothing,
      location = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
layerVersionContentOutput_signingProfileVersionArn :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
layerVersionContentOutput_signingProfileVersionArn = Lens.lens (\LayerVersionContentOutput' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@LayerVersionContentOutput' {} a -> s {signingProfileVersionArn = a} :: LayerVersionContentOutput)

-- | The SHA-256 hash of the layer archive.
layerVersionContentOutput_codeSha256 :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
layerVersionContentOutput_codeSha256 = Lens.lens (\LayerVersionContentOutput' {codeSha256} -> codeSha256) (\s@LayerVersionContentOutput' {} a -> s {codeSha256 = a} :: LayerVersionContentOutput)

-- | The Amazon Resource Name (ARN) of a signing job.
layerVersionContentOutput_signingJobArn :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
layerVersionContentOutput_signingJobArn = Lens.lens (\LayerVersionContentOutput' {signingJobArn} -> signingJobArn) (\s@LayerVersionContentOutput' {} a -> s {signingJobArn = a} :: LayerVersionContentOutput)

-- | The size of the layer archive in bytes.
layerVersionContentOutput_codeSize :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Integer)
layerVersionContentOutput_codeSize = Lens.lens (\LayerVersionContentOutput' {codeSize} -> codeSize) (\s@LayerVersionContentOutput' {} a -> s {codeSize = a} :: LayerVersionContentOutput)

-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
layerVersionContentOutput_location :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
layerVersionContentOutput_location = Lens.lens (\LayerVersionContentOutput' {location} -> location) (\s@LayerVersionContentOutput' {} a -> s {location = a} :: LayerVersionContentOutput)

instance Core.FromJSON LayerVersionContentOutput where
  parseJSON =
    Core.withObject
      "LayerVersionContentOutput"
      ( \x ->
          LayerVersionContentOutput'
            Core.<$> (x Core..:? "SigningProfileVersionArn")
            Core.<*> (x Core..:? "CodeSha256")
            Core.<*> (x Core..:? "SigningJobArn")
            Core.<*> (x Core..:? "CodeSize")
            Core.<*> (x Core..:? "Location")
      )

instance Core.Hashable LayerVersionContentOutput

instance Core.NFData LayerVersionContentOutput

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
-- Module      : Network.AWS.SageMaker.Types.ModelBiasAppSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelBiasAppSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Docker container image configuration object for the model bias job.
--
-- /See:/ 'newModelBiasAppSpecification' smart constructor.
data ModelBiasAppSpecification = ModelBiasAppSpecification'
  { -- | Sets the environment variables in the Docker container.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The container image to be run by the model bias job.
    imageUri :: Core.Text,
    -- | JSON formatted S3 file that defines bias parameters. For more
    -- information on this JSON configuration file, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/json-bias-parameter-config.html Configure bias parameters>.
    configUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelBiasAppSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'modelBiasAppSpecification_environment' - Sets the environment variables in the Docker container.
--
-- 'imageUri', 'modelBiasAppSpecification_imageUri' - The container image to be run by the model bias job.
--
-- 'configUri', 'modelBiasAppSpecification_configUri' - JSON formatted S3 file that defines bias parameters. For more
-- information on this JSON configuration file, see
-- <https://docs.aws.amazon.com/sagemaker/latest/json-bias-parameter-config.html Configure bias parameters>.
newModelBiasAppSpecification ::
  -- | 'imageUri'
  Core.Text ->
  -- | 'configUri'
  Core.Text ->
  ModelBiasAppSpecification
newModelBiasAppSpecification pImageUri_ pConfigUri_ =
  ModelBiasAppSpecification'
    { environment =
        Core.Nothing,
      imageUri = pImageUri_,
      configUri = pConfigUri_
    }

-- | Sets the environment variables in the Docker container.
modelBiasAppSpecification_environment :: Lens.Lens' ModelBiasAppSpecification (Core.Maybe (Core.HashMap Core.Text Core.Text))
modelBiasAppSpecification_environment = Lens.lens (\ModelBiasAppSpecification' {environment} -> environment) (\s@ModelBiasAppSpecification' {} a -> s {environment = a} :: ModelBiasAppSpecification) Core.. Lens.mapping Lens._Coerce

-- | The container image to be run by the model bias job.
modelBiasAppSpecification_imageUri :: Lens.Lens' ModelBiasAppSpecification Core.Text
modelBiasAppSpecification_imageUri = Lens.lens (\ModelBiasAppSpecification' {imageUri} -> imageUri) (\s@ModelBiasAppSpecification' {} a -> s {imageUri = a} :: ModelBiasAppSpecification)

-- | JSON formatted S3 file that defines bias parameters. For more
-- information on this JSON configuration file, see
-- <https://docs.aws.amazon.com/sagemaker/latest/json-bias-parameter-config.html Configure bias parameters>.
modelBiasAppSpecification_configUri :: Lens.Lens' ModelBiasAppSpecification Core.Text
modelBiasAppSpecification_configUri = Lens.lens (\ModelBiasAppSpecification' {configUri} -> configUri) (\s@ModelBiasAppSpecification' {} a -> s {configUri = a} :: ModelBiasAppSpecification)

instance Core.FromJSON ModelBiasAppSpecification where
  parseJSON =
    Core.withObject
      "ModelBiasAppSpecification"
      ( \x ->
          ModelBiasAppSpecification'
            Core.<$> (x Core..:? "Environment" Core..!= Core.mempty)
            Core.<*> (x Core..: "ImageUri")
            Core.<*> (x Core..: "ConfigUri")
      )

instance Core.Hashable ModelBiasAppSpecification

instance Core.NFData ModelBiasAppSpecification

instance Core.ToJSON ModelBiasAppSpecification where
  toJSON ModelBiasAppSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Environment" Core..=) Core.<$> environment,
            Core.Just ("ImageUri" Core..= imageUri),
            Core.Just ("ConfigUri" Core..= configUri)
          ]
      )

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
import qualified Network.AWS.Prelude as Prelude

-- | Docker container image configuration object for the model bias job.
--
-- /See:/ 'newModelBiasAppSpecification' smart constructor.
data ModelBiasAppSpecification = ModelBiasAppSpecification'
  { -- | Sets the environment variables in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The container image to be run by the model bias job.
    imageUri :: Prelude.Text,
    -- | JSON formatted S3 file that defines bias parameters. For more
    -- information on this JSON configuration file, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/json-bias-parameter-config.html Configure bias parameters>.
    configUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'configUri'
  Prelude.Text ->
  ModelBiasAppSpecification
newModelBiasAppSpecification pImageUri_ pConfigUri_ =
  ModelBiasAppSpecification'
    { environment =
        Prelude.Nothing,
      imageUri = pImageUri_,
      configUri = pConfigUri_
    }

-- | Sets the environment variables in the Docker container.
modelBiasAppSpecification_environment :: Lens.Lens' ModelBiasAppSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelBiasAppSpecification_environment = Lens.lens (\ModelBiasAppSpecification' {environment} -> environment) (\s@ModelBiasAppSpecification' {} a -> s {environment = a} :: ModelBiasAppSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | The container image to be run by the model bias job.
modelBiasAppSpecification_imageUri :: Lens.Lens' ModelBiasAppSpecification Prelude.Text
modelBiasAppSpecification_imageUri = Lens.lens (\ModelBiasAppSpecification' {imageUri} -> imageUri) (\s@ModelBiasAppSpecification' {} a -> s {imageUri = a} :: ModelBiasAppSpecification)

-- | JSON formatted S3 file that defines bias parameters. For more
-- information on this JSON configuration file, see
-- <https://docs.aws.amazon.com/sagemaker/latest/json-bias-parameter-config.html Configure bias parameters>.
modelBiasAppSpecification_configUri :: Lens.Lens' ModelBiasAppSpecification Prelude.Text
modelBiasAppSpecification_configUri = Lens.lens (\ModelBiasAppSpecification' {configUri} -> configUri) (\s@ModelBiasAppSpecification' {} a -> s {configUri = a} :: ModelBiasAppSpecification)

instance Core.FromJSON ModelBiasAppSpecification where
  parseJSON =
    Core.withObject
      "ModelBiasAppSpecification"
      ( \x ->
          ModelBiasAppSpecification'
            Prelude.<$> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "ImageUri")
            Prelude.<*> (x Core..: "ConfigUri")
      )

instance Prelude.Hashable ModelBiasAppSpecification

instance Prelude.NFData ModelBiasAppSpecification

instance Core.ToJSON ModelBiasAppSpecification where
  toJSON ModelBiasAppSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Environment" Core..=) Prelude.<$> environment,
            Prelude.Just ("ImageUri" Core..= imageUri),
            Prelude.Just ("ConfigUri" Core..= configUri)
          ]
      )

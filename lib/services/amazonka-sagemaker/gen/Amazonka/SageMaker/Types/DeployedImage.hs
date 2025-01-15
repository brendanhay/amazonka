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
-- Module      : Amazonka.SageMaker.Types.DeployedImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeployedImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Gets the Amazon EC2 Container Registry path of the docker image of the
-- model that is hosted in this ProductionVariant.
--
-- If you used the @registry\/repository[:tag]@ form to specify the image
-- path of the primary container when you created the model hosted in this
-- @ProductionVariant@, the path resolves to a path of the form
-- @registry\/repository[\@digest]@. A digest is a hash value that
-- identifies a specific version of an image. For information about Amazon
-- ECR paths, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an Image>
-- in the /Amazon ECR User Guide/.
--
-- /See:/ 'newDeployedImage' smart constructor.
data DeployedImage = DeployedImage'
  { -- | The date and time when the image path for the model resolved to the
    -- @ResolvedImage@
    resolutionTime :: Prelude.Maybe Data.POSIX,
    -- | The specific digest path of the image hosted in this
    -- @ProductionVariant@.
    resolvedImage :: Prelude.Maybe Prelude.Text,
    -- | The image path you specified when you created the model.
    specifiedImage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeployedImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolutionTime', 'deployedImage_resolutionTime' - The date and time when the image path for the model resolved to the
-- @ResolvedImage@
--
-- 'resolvedImage', 'deployedImage_resolvedImage' - The specific digest path of the image hosted in this
-- @ProductionVariant@.
--
-- 'specifiedImage', 'deployedImage_specifiedImage' - The image path you specified when you created the model.
newDeployedImage ::
  DeployedImage
newDeployedImage =
  DeployedImage'
    { resolutionTime = Prelude.Nothing,
      resolvedImage = Prelude.Nothing,
      specifiedImage = Prelude.Nothing
    }

-- | The date and time when the image path for the model resolved to the
-- @ResolvedImage@
deployedImage_resolutionTime :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.UTCTime)
deployedImage_resolutionTime = Lens.lens (\DeployedImage' {resolutionTime} -> resolutionTime) (\s@DeployedImage' {} a -> s {resolutionTime = a} :: DeployedImage) Prelude.. Lens.mapping Data._Time

-- | The specific digest path of the image hosted in this
-- @ProductionVariant@.
deployedImage_resolvedImage :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.Text)
deployedImage_resolvedImage = Lens.lens (\DeployedImage' {resolvedImage} -> resolvedImage) (\s@DeployedImage' {} a -> s {resolvedImage = a} :: DeployedImage)

-- | The image path you specified when you created the model.
deployedImage_specifiedImage :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.Text)
deployedImage_specifiedImage = Lens.lens (\DeployedImage' {specifiedImage} -> specifiedImage) (\s@DeployedImage' {} a -> s {specifiedImage = a} :: DeployedImage)

instance Data.FromJSON DeployedImage where
  parseJSON =
    Data.withObject
      "DeployedImage"
      ( \x ->
          DeployedImage'
            Prelude.<$> (x Data..:? "ResolutionTime")
            Prelude.<*> (x Data..:? "ResolvedImage")
            Prelude.<*> (x Data..:? "SpecifiedImage")
      )

instance Prelude.Hashable DeployedImage where
  hashWithSalt _salt DeployedImage' {..} =
    _salt
      `Prelude.hashWithSalt` resolutionTime
      `Prelude.hashWithSalt` resolvedImage
      `Prelude.hashWithSalt` specifiedImage

instance Prelude.NFData DeployedImage where
  rnf DeployedImage' {..} =
    Prelude.rnf resolutionTime `Prelude.seq`
      Prelude.rnf resolvedImage `Prelude.seq`
        Prelude.rnf specifiedImage

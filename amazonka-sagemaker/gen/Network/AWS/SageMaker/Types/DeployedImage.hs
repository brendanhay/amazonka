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
-- Module      : Network.AWS.SageMaker.Types.DeployedImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeployedImage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The image path you specified when you created the model.
    specifiedImage :: Prelude.Maybe Prelude.Text,
    -- | The specific digest path of the image hosted in this
    -- @ProductionVariant@.
    resolvedImage :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the image path for the model resolved to the
    -- @ResolvedImage@
    resolutionTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeployedImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'specifiedImage', 'deployedImage_specifiedImage' - The image path you specified when you created the model.
--
-- 'resolvedImage', 'deployedImage_resolvedImage' - The specific digest path of the image hosted in this
-- @ProductionVariant@.
--
-- 'resolutionTime', 'deployedImage_resolutionTime' - The date and time when the image path for the model resolved to the
-- @ResolvedImage@
newDeployedImage ::
  DeployedImage
newDeployedImage =
  DeployedImage'
    { specifiedImage = Prelude.Nothing,
      resolvedImage = Prelude.Nothing,
      resolutionTime = Prelude.Nothing
    }

-- | The image path you specified when you created the model.
deployedImage_specifiedImage :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.Text)
deployedImage_specifiedImage = Lens.lens (\DeployedImage' {specifiedImage} -> specifiedImage) (\s@DeployedImage' {} a -> s {specifiedImage = a} :: DeployedImage)

-- | The specific digest path of the image hosted in this
-- @ProductionVariant@.
deployedImage_resolvedImage :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.Text)
deployedImage_resolvedImage = Lens.lens (\DeployedImage' {resolvedImage} -> resolvedImage) (\s@DeployedImage' {} a -> s {resolvedImage = a} :: DeployedImage)

-- | The date and time when the image path for the model resolved to the
-- @ResolvedImage@
deployedImage_resolutionTime :: Lens.Lens' DeployedImage (Prelude.Maybe Prelude.UTCTime)
deployedImage_resolutionTime = Lens.lens (\DeployedImage' {resolutionTime} -> resolutionTime) (\s@DeployedImage' {} a -> s {resolutionTime = a} :: DeployedImage) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON DeployedImage where
  parseJSON =
    Prelude.withObject
      "DeployedImage"
      ( \x ->
          DeployedImage'
            Prelude.<$> (x Prelude..:? "SpecifiedImage")
            Prelude.<*> (x Prelude..:? "ResolvedImage")
            Prelude.<*> (x Prelude..:? "ResolutionTime")
      )

instance Prelude.Hashable DeployedImage

instance Prelude.NFData DeployedImage

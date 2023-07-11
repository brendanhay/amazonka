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
-- Module      : Amazonka.Inspector2.Types.ResourceScanMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceScanMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Ec2Metadata
import Amazonka.Inspector2.Types.EcrContainerImageMetadata
import Amazonka.Inspector2.Types.EcrRepositoryMetadata
import Amazonka.Inspector2.Types.LambdaFunctionMetadata
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about the metadata for an Amazon ECR
-- resource.
--
-- /See:/ 'newResourceScanMetadata' smart constructor.
data ResourceScanMetadata = ResourceScanMetadata'
  { -- | An object that contains metadata details for an Amazon EC2 instance.
    ec2 :: Prelude.Maybe Ec2Metadata,
    -- | An object that contains details about the container metadata for an
    -- Amazon ECR image.
    ecrImage :: Prelude.Maybe EcrContainerImageMetadata,
    -- | An object that contains details about the repository an Amazon ECR image
    -- resides in.
    ecrRepository :: Prelude.Maybe EcrRepositoryMetadata,
    -- | An object that contains metadata details for an AWS Lambda function.
    lambdaFunction :: Prelude.Maybe LambdaFunctionMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceScanMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2', 'resourceScanMetadata_ec2' - An object that contains metadata details for an Amazon EC2 instance.
--
-- 'ecrImage', 'resourceScanMetadata_ecrImage' - An object that contains details about the container metadata for an
-- Amazon ECR image.
--
-- 'ecrRepository', 'resourceScanMetadata_ecrRepository' - An object that contains details about the repository an Amazon ECR image
-- resides in.
--
-- 'lambdaFunction', 'resourceScanMetadata_lambdaFunction' - An object that contains metadata details for an AWS Lambda function.
newResourceScanMetadata ::
  ResourceScanMetadata
newResourceScanMetadata =
  ResourceScanMetadata'
    { ec2 = Prelude.Nothing,
      ecrImage = Prelude.Nothing,
      ecrRepository = Prelude.Nothing,
      lambdaFunction = Prelude.Nothing
    }

-- | An object that contains metadata details for an Amazon EC2 instance.
resourceScanMetadata_ec2 :: Lens.Lens' ResourceScanMetadata (Prelude.Maybe Ec2Metadata)
resourceScanMetadata_ec2 = Lens.lens (\ResourceScanMetadata' {ec2} -> ec2) (\s@ResourceScanMetadata' {} a -> s {ec2 = a} :: ResourceScanMetadata)

-- | An object that contains details about the container metadata for an
-- Amazon ECR image.
resourceScanMetadata_ecrImage :: Lens.Lens' ResourceScanMetadata (Prelude.Maybe EcrContainerImageMetadata)
resourceScanMetadata_ecrImage = Lens.lens (\ResourceScanMetadata' {ecrImage} -> ecrImage) (\s@ResourceScanMetadata' {} a -> s {ecrImage = a} :: ResourceScanMetadata)

-- | An object that contains details about the repository an Amazon ECR image
-- resides in.
resourceScanMetadata_ecrRepository :: Lens.Lens' ResourceScanMetadata (Prelude.Maybe EcrRepositoryMetadata)
resourceScanMetadata_ecrRepository = Lens.lens (\ResourceScanMetadata' {ecrRepository} -> ecrRepository) (\s@ResourceScanMetadata' {} a -> s {ecrRepository = a} :: ResourceScanMetadata)

-- | An object that contains metadata details for an AWS Lambda function.
resourceScanMetadata_lambdaFunction :: Lens.Lens' ResourceScanMetadata (Prelude.Maybe LambdaFunctionMetadata)
resourceScanMetadata_lambdaFunction = Lens.lens (\ResourceScanMetadata' {lambdaFunction} -> lambdaFunction) (\s@ResourceScanMetadata' {} a -> s {lambdaFunction = a} :: ResourceScanMetadata)

instance Data.FromJSON ResourceScanMetadata where
  parseJSON =
    Data.withObject
      "ResourceScanMetadata"
      ( \x ->
          ResourceScanMetadata'
            Prelude.<$> (x Data..:? "ec2")
            Prelude.<*> (x Data..:? "ecrImage")
            Prelude.<*> (x Data..:? "ecrRepository")
            Prelude.<*> (x Data..:? "lambdaFunction")
      )

instance Prelude.Hashable ResourceScanMetadata where
  hashWithSalt _salt ResourceScanMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` ec2
      `Prelude.hashWithSalt` ecrImage
      `Prelude.hashWithSalt` ecrRepository
      `Prelude.hashWithSalt` lambdaFunction

instance Prelude.NFData ResourceScanMetadata where
  rnf ResourceScanMetadata' {..} =
    Prelude.rnf ec2
      `Prelude.seq` Prelude.rnf ecrImage
      `Prelude.seq` Prelude.rnf ecrRepository
      `Prelude.seq` Prelude.rnf lambdaFunction

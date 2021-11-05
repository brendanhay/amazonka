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
-- Module      : Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails

-- | Provides information about an Amazon Elastic Container Registry
-- repository.
--
-- /See:/ 'newAwsEcrRepositoryDetails' smart constructor.
data AwsEcrRepositoryDetails = AwsEcrRepositoryDetails'
  { -- | The ARN of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The image scanning configuration for a repository.
    imageScanningConfiguration :: Prelude.Maybe AwsEcrRepositoryImageScanningConfigurationDetails,
    -- | The text of the repository policy.
    repositoryPolicyText :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The tag mutability setting for the repository.
    imageTagMutability :: Prelude.Maybe Prelude.Text,
    -- | Information about the lifecycle policy for the repository.
    lifecyclePolicy :: Prelude.Maybe AwsEcrRepositoryLifecyclePolicyDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrRepositoryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsEcrRepositoryDetails_arn' - The ARN of the repository.
--
-- 'imageScanningConfiguration', 'awsEcrRepositoryDetails_imageScanningConfiguration' - The image scanning configuration for a repository.
--
-- 'repositoryPolicyText', 'awsEcrRepositoryDetails_repositoryPolicyText' - The text of the repository policy.
--
-- 'repositoryName', 'awsEcrRepositoryDetails_repositoryName' - The name of the repository.
--
-- 'imageTagMutability', 'awsEcrRepositoryDetails_imageTagMutability' - The tag mutability setting for the repository.
--
-- 'lifecyclePolicy', 'awsEcrRepositoryDetails_lifecyclePolicy' - Information about the lifecycle policy for the repository.
newAwsEcrRepositoryDetails ::
  AwsEcrRepositoryDetails
newAwsEcrRepositoryDetails =
  AwsEcrRepositoryDetails'
    { arn = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing,
      repositoryPolicyText = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      imageTagMutability = Prelude.Nothing,
      lifecyclePolicy = Prelude.Nothing
    }

-- | The ARN of the repository.
awsEcrRepositoryDetails_arn :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_arn = Lens.lens (\AwsEcrRepositoryDetails' {arn} -> arn) (\s@AwsEcrRepositoryDetails' {} a -> s {arn = a} :: AwsEcrRepositoryDetails)

-- | The image scanning configuration for a repository.
awsEcrRepositoryDetails_imageScanningConfiguration :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe AwsEcrRepositoryImageScanningConfigurationDetails)
awsEcrRepositoryDetails_imageScanningConfiguration = Lens.lens (\AwsEcrRepositoryDetails' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@AwsEcrRepositoryDetails' {} a -> s {imageScanningConfiguration = a} :: AwsEcrRepositoryDetails)

-- | The text of the repository policy.
awsEcrRepositoryDetails_repositoryPolicyText :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_repositoryPolicyText = Lens.lens (\AwsEcrRepositoryDetails' {repositoryPolicyText} -> repositoryPolicyText) (\s@AwsEcrRepositoryDetails' {} a -> s {repositoryPolicyText = a} :: AwsEcrRepositoryDetails)

-- | The name of the repository.
awsEcrRepositoryDetails_repositoryName :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_repositoryName = Lens.lens (\AwsEcrRepositoryDetails' {repositoryName} -> repositoryName) (\s@AwsEcrRepositoryDetails' {} a -> s {repositoryName = a} :: AwsEcrRepositoryDetails)

-- | The tag mutability setting for the repository.
awsEcrRepositoryDetails_imageTagMutability :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_imageTagMutability = Lens.lens (\AwsEcrRepositoryDetails' {imageTagMutability} -> imageTagMutability) (\s@AwsEcrRepositoryDetails' {} a -> s {imageTagMutability = a} :: AwsEcrRepositoryDetails)

-- | Information about the lifecycle policy for the repository.
awsEcrRepositoryDetails_lifecyclePolicy :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe AwsEcrRepositoryLifecyclePolicyDetails)
awsEcrRepositoryDetails_lifecyclePolicy = Lens.lens (\AwsEcrRepositoryDetails' {lifecyclePolicy} -> lifecyclePolicy) (\s@AwsEcrRepositoryDetails' {} a -> s {lifecyclePolicy = a} :: AwsEcrRepositoryDetails)

instance Core.FromJSON AwsEcrRepositoryDetails where
  parseJSON =
    Core.withObject
      "AwsEcrRepositoryDetails"
      ( \x ->
          AwsEcrRepositoryDetails'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "ImageScanningConfiguration")
            Prelude.<*> (x Core..:? "RepositoryPolicyText")
            Prelude.<*> (x Core..:? "RepositoryName")
            Prelude.<*> (x Core..:? "ImageTagMutability")
            Prelude.<*> (x Core..:? "LifecyclePolicy")
      )

instance Prelude.Hashable AwsEcrRepositoryDetails

instance Prelude.NFData AwsEcrRepositoryDetails

instance Core.ToJSON AwsEcrRepositoryDetails where
  toJSON AwsEcrRepositoryDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Arn" Core..=) Prelude.<$> arn,
            ("ImageScanningConfiguration" Core..=)
              Prelude.<$> imageScanningConfiguration,
            ("RepositoryPolicyText" Core..=)
              Prelude.<$> repositoryPolicyText,
            ("RepositoryName" Core..=)
              Prelude.<$> repositoryName,
            ("ImageTagMutability" Core..=)
              Prelude.<$> imageTagMutability,
            ("LifecyclePolicy" Core..=)
              Prelude.<$> lifecyclePolicy
          ]
      )

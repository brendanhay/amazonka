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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails

-- | Provides information about an Amazon Elastic Container Registry
-- repository.
--
-- /See:/ 'newAwsEcrRepositoryDetails' smart constructor.
data AwsEcrRepositoryDetails = AwsEcrRepositoryDetails'
  { -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The text of the repository policy.
    repositoryPolicyText :: Prelude.Maybe Prelude.Text,
    -- | Information about the lifecycle policy for the repository.
    lifecyclePolicy :: Prelude.Maybe AwsEcrRepositoryLifecyclePolicyDetails,
    -- | The tag mutability setting for the repository. Valid values are
    -- @IMMUTABLE@ or @MUTABLE@.
    imageTagMutability :: Prelude.Maybe Prelude.Text,
    -- | The image scanning configuration for a repository.
    imageScanningConfiguration :: Prelude.Maybe AwsEcrRepositoryImageScanningConfigurationDetails
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
-- 'repositoryName', 'awsEcrRepositoryDetails_repositoryName' - The name of the repository.
--
-- 'arn', 'awsEcrRepositoryDetails_arn' - The ARN of the repository.
--
-- 'repositoryPolicyText', 'awsEcrRepositoryDetails_repositoryPolicyText' - The text of the repository policy.
--
-- 'lifecyclePolicy', 'awsEcrRepositoryDetails_lifecyclePolicy' - Information about the lifecycle policy for the repository.
--
-- 'imageTagMutability', 'awsEcrRepositoryDetails_imageTagMutability' - The tag mutability setting for the repository. Valid values are
-- @IMMUTABLE@ or @MUTABLE@.
--
-- 'imageScanningConfiguration', 'awsEcrRepositoryDetails_imageScanningConfiguration' - The image scanning configuration for a repository.
newAwsEcrRepositoryDetails ::
  AwsEcrRepositoryDetails
newAwsEcrRepositoryDetails =
  AwsEcrRepositoryDetails'
    { repositoryName =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      repositoryPolicyText = Prelude.Nothing,
      lifecyclePolicy = Prelude.Nothing,
      imageTagMutability = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing
    }

-- | The name of the repository.
awsEcrRepositoryDetails_repositoryName :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_repositoryName = Lens.lens (\AwsEcrRepositoryDetails' {repositoryName} -> repositoryName) (\s@AwsEcrRepositoryDetails' {} a -> s {repositoryName = a} :: AwsEcrRepositoryDetails)

-- | The ARN of the repository.
awsEcrRepositoryDetails_arn :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_arn = Lens.lens (\AwsEcrRepositoryDetails' {arn} -> arn) (\s@AwsEcrRepositoryDetails' {} a -> s {arn = a} :: AwsEcrRepositoryDetails)

-- | The text of the repository policy.
awsEcrRepositoryDetails_repositoryPolicyText :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_repositoryPolicyText = Lens.lens (\AwsEcrRepositoryDetails' {repositoryPolicyText} -> repositoryPolicyText) (\s@AwsEcrRepositoryDetails' {} a -> s {repositoryPolicyText = a} :: AwsEcrRepositoryDetails)

-- | Information about the lifecycle policy for the repository.
awsEcrRepositoryDetails_lifecyclePolicy :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe AwsEcrRepositoryLifecyclePolicyDetails)
awsEcrRepositoryDetails_lifecyclePolicy = Lens.lens (\AwsEcrRepositoryDetails' {lifecyclePolicy} -> lifecyclePolicy) (\s@AwsEcrRepositoryDetails' {} a -> s {lifecyclePolicy = a} :: AwsEcrRepositoryDetails)

-- | The tag mutability setting for the repository. Valid values are
-- @IMMUTABLE@ or @MUTABLE@.
awsEcrRepositoryDetails_imageTagMutability :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryDetails_imageTagMutability = Lens.lens (\AwsEcrRepositoryDetails' {imageTagMutability} -> imageTagMutability) (\s@AwsEcrRepositoryDetails' {} a -> s {imageTagMutability = a} :: AwsEcrRepositoryDetails)

-- | The image scanning configuration for a repository.
awsEcrRepositoryDetails_imageScanningConfiguration :: Lens.Lens' AwsEcrRepositoryDetails (Prelude.Maybe AwsEcrRepositoryImageScanningConfigurationDetails)
awsEcrRepositoryDetails_imageScanningConfiguration = Lens.lens (\AwsEcrRepositoryDetails' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@AwsEcrRepositoryDetails' {} a -> s {imageScanningConfiguration = a} :: AwsEcrRepositoryDetails)

instance Core.FromJSON AwsEcrRepositoryDetails where
  parseJSON =
    Core.withObject
      "AwsEcrRepositoryDetails"
      ( \x ->
          AwsEcrRepositoryDetails'
            Prelude.<$> (x Core..:? "RepositoryName")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "RepositoryPolicyText")
            Prelude.<*> (x Core..:? "LifecyclePolicy")
            Prelude.<*> (x Core..:? "ImageTagMutability")
            Prelude.<*> (x Core..:? "ImageScanningConfiguration")
      )

instance Prelude.Hashable AwsEcrRepositoryDetails where
  hashWithSalt _salt AwsEcrRepositoryDetails' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` repositoryPolicyText
      `Prelude.hashWithSalt` lifecyclePolicy
      `Prelude.hashWithSalt` imageTagMutability
      `Prelude.hashWithSalt` imageScanningConfiguration

instance Prelude.NFData AwsEcrRepositoryDetails where
  rnf AwsEcrRepositoryDetails' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf repositoryPolicyText
      `Prelude.seq` Prelude.rnf lifecyclePolicy
      `Prelude.seq` Prelude.rnf imageTagMutability
      `Prelude.seq` Prelude.rnf imageScanningConfiguration

instance Core.ToJSON AwsEcrRepositoryDetails where
  toJSON AwsEcrRepositoryDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RepositoryName" Core..=)
              Prelude.<$> repositoryName,
            ("Arn" Core..=) Prelude.<$> arn,
            ("RepositoryPolicyText" Core..=)
              Prelude.<$> repositoryPolicyText,
            ("LifecyclePolicy" Core..=)
              Prelude.<$> lifecyclePolicy,
            ("ImageTagMutability" Core..=)
              Prelude.<$> imageTagMutability,
            ("ImageScanningConfiguration" Core..=)
              Prelude.<$> imageScanningConfiguration
          ]
      )

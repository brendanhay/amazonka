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
-- Module      : Amazonka.ECR.Types.AwsEcrContainerImageDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.AwsEcrContainerImageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The image details of the Amazon ECR container image.
--
-- /See:/ 'newAwsEcrContainerImageDetails' smart constructor.
data AwsEcrContainerImageDetails = AwsEcrContainerImageDetails'
  { -- | The architecture of the Amazon ECR container image.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The image author of the Amazon ECR container image.
    author :: Prelude.Maybe Prelude.Text,
    -- | The image hash of the Amazon ECR container image.
    imageHash :: Prelude.Maybe Prelude.Text,
    -- | The image tags attached to the Amazon ECR container image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The platform of the Amazon ECR container image.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Amazon ECR container image was pushed.
    pushedAt :: Prelude.Maybe Data.POSIX,
    -- | The registry the Amazon ECR container image belongs to.
    registry :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository the Amazon ECR container image resides in.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrContainerImageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'awsEcrContainerImageDetails_architecture' - The architecture of the Amazon ECR container image.
--
-- 'author', 'awsEcrContainerImageDetails_author' - The image author of the Amazon ECR container image.
--
-- 'imageHash', 'awsEcrContainerImageDetails_imageHash' - The image hash of the Amazon ECR container image.
--
-- 'imageTags', 'awsEcrContainerImageDetails_imageTags' - The image tags attached to the Amazon ECR container image.
--
-- 'platform', 'awsEcrContainerImageDetails_platform' - The platform of the Amazon ECR container image.
--
-- 'pushedAt', 'awsEcrContainerImageDetails_pushedAt' - The date and time the Amazon ECR container image was pushed.
--
-- 'registry', 'awsEcrContainerImageDetails_registry' - The registry the Amazon ECR container image belongs to.
--
-- 'repositoryName', 'awsEcrContainerImageDetails_repositoryName' - The name of the repository the Amazon ECR container image resides in.
newAwsEcrContainerImageDetails ::
  AwsEcrContainerImageDetails
newAwsEcrContainerImageDetails =
  AwsEcrContainerImageDetails'
    { architecture =
        Prelude.Nothing,
      author = Prelude.Nothing,
      imageHash = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      platform = Prelude.Nothing,
      pushedAt = Prelude.Nothing,
      registry = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The architecture of the Amazon ECR container image.
awsEcrContainerImageDetails_architecture :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_architecture = Lens.lens (\AwsEcrContainerImageDetails' {architecture} -> architecture) (\s@AwsEcrContainerImageDetails' {} a -> s {architecture = a} :: AwsEcrContainerImageDetails)

-- | The image author of the Amazon ECR container image.
awsEcrContainerImageDetails_author :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_author = Lens.lens (\AwsEcrContainerImageDetails' {author} -> author) (\s@AwsEcrContainerImageDetails' {} a -> s {author = a} :: AwsEcrContainerImageDetails)

-- | The image hash of the Amazon ECR container image.
awsEcrContainerImageDetails_imageHash :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_imageHash = Lens.lens (\AwsEcrContainerImageDetails' {imageHash} -> imageHash) (\s@AwsEcrContainerImageDetails' {} a -> s {imageHash = a} :: AwsEcrContainerImageDetails)

-- | The image tags attached to the Amazon ECR container image.
awsEcrContainerImageDetails_imageTags :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe [Prelude.Text])
awsEcrContainerImageDetails_imageTags = Lens.lens (\AwsEcrContainerImageDetails' {imageTags} -> imageTags) (\s@AwsEcrContainerImageDetails' {} a -> s {imageTags = a} :: AwsEcrContainerImageDetails) Prelude.. Lens.mapping Lens.coerced

-- | The platform of the Amazon ECR container image.
awsEcrContainerImageDetails_platform :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_platform = Lens.lens (\AwsEcrContainerImageDetails' {platform} -> platform) (\s@AwsEcrContainerImageDetails' {} a -> s {platform = a} :: AwsEcrContainerImageDetails)

-- | The date and time the Amazon ECR container image was pushed.
awsEcrContainerImageDetails_pushedAt :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.UTCTime)
awsEcrContainerImageDetails_pushedAt = Lens.lens (\AwsEcrContainerImageDetails' {pushedAt} -> pushedAt) (\s@AwsEcrContainerImageDetails' {} a -> s {pushedAt = a} :: AwsEcrContainerImageDetails) Prelude.. Lens.mapping Data._Time

-- | The registry the Amazon ECR container image belongs to.
awsEcrContainerImageDetails_registry :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_registry = Lens.lens (\AwsEcrContainerImageDetails' {registry} -> registry) (\s@AwsEcrContainerImageDetails' {} a -> s {registry = a} :: AwsEcrContainerImageDetails)

-- | The name of the repository the Amazon ECR container image resides in.
awsEcrContainerImageDetails_repositoryName :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_repositoryName = Lens.lens (\AwsEcrContainerImageDetails' {repositoryName} -> repositoryName) (\s@AwsEcrContainerImageDetails' {} a -> s {repositoryName = a} :: AwsEcrContainerImageDetails)

instance Data.FromJSON AwsEcrContainerImageDetails where
  parseJSON =
    Data.withObject
      "AwsEcrContainerImageDetails"
      ( \x ->
          AwsEcrContainerImageDetails'
            Prelude.<$> (x Data..:? "architecture")
            Prelude.<*> (x Data..:? "author")
            Prelude.<*> (x Data..:? "imageHash")
            Prelude.<*> (x Data..:? "imageTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "pushedAt")
            Prelude.<*> (x Data..:? "registry")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable AwsEcrContainerImageDetails where
  hashWithSalt _salt AwsEcrContainerImageDetails' {..} =
    _salt `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` imageHash
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` pushedAt
      `Prelude.hashWithSalt` registry
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData AwsEcrContainerImageDetails where
  rnf AwsEcrContainerImageDetails' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf imageHash
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf pushedAt
      `Prelude.seq` Prelude.rnf registry
      `Prelude.seq` Prelude.rnf repositoryName

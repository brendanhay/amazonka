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
-- Module      : Network.AWS.SecurityHub.Types.AwsEcrContainerImageDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEcrContainerImageDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Amazon ECR image.
--
-- /See:/ 'newAwsEcrContainerImageDetails' smart constructor.
data AwsEcrContainerImageDetails = AwsEcrContainerImageDetails'
  { -- | The Amazon Web Services account identifier that is associated with the
    -- registry that the image belongs to.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The list of tags that are associated with the image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The sha256 digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the image was pushed to the repository.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    imagePublishedAt :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the image.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that the image belongs to.
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
-- 'registryId', 'awsEcrContainerImageDetails_registryId' - The Amazon Web Services account identifier that is associated with the
-- registry that the image belongs to.
--
-- 'imageTags', 'awsEcrContainerImageDetails_imageTags' - The list of tags that are associated with the image.
--
-- 'imageDigest', 'awsEcrContainerImageDetails_imageDigest' - The sha256 digest of the image manifest.
--
-- 'imagePublishedAt', 'awsEcrContainerImageDetails_imagePublishedAt' - The date and time when the image was pushed to the repository.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'architecture', 'awsEcrContainerImageDetails_architecture' - The architecture of the image.
--
-- 'repositoryName', 'awsEcrContainerImageDetails_repositoryName' - The name of the repository that the image belongs to.
newAwsEcrContainerImageDetails ::
  AwsEcrContainerImageDetails
newAwsEcrContainerImageDetails =
  AwsEcrContainerImageDetails'
    { registryId =
        Prelude.Nothing,
      imageTags = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      imagePublishedAt = Prelude.Nothing,
      architecture = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The Amazon Web Services account identifier that is associated with the
-- registry that the image belongs to.
awsEcrContainerImageDetails_registryId :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_registryId = Lens.lens (\AwsEcrContainerImageDetails' {registryId} -> registryId) (\s@AwsEcrContainerImageDetails' {} a -> s {registryId = a} :: AwsEcrContainerImageDetails)

-- | The list of tags that are associated with the image.
awsEcrContainerImageDetails_imageTags :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe [Prelude.Text])
awsEcrContainerImageDetails_imageTags = Lens.lens (\AwsEcrContainerImageDetails' {imageTags} -> imageTags) (\s@AwsEcrContainerImageDetails' {} a -> s {imageTags = a} :: AwsEcrContainerImageDetails) Prelude.. Lens.mapping Lens.coerced

-- | The sha256 digest of the image manifest.
awsEcrContainerImageDetails_imageDigest :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_imageDigest = Lens.lens (\AwsEcrContainerImageDetails' {imageDigest} -> imageDigest) (\s@AwsEcrContainerImageDetails' {} a -> s {imageDigest = a} :: AwsEcrContainerImageDetails)

-- | The date and time when the image was pushed to the repository.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEcrContainerImageDetails_imagePublishedAt :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_imagePublishedAt = Lens.lens (\AwsEcrContainerImageDetails' {imagePublishedAt} -> imagePublishedAt) (\s@AwsEcrContainerImageDetails' {} a -> s {imagePublishedAt = a} :: AwsEcrContainerImageDetails)

-- | The architecture of the image.
awsEcrContainerImageDetails_architecture :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_architecture = Lens.lens (\AwsEcrContainerImageDetails' {architecture} -> architecture) (\s@AwsEcrContainerImageDetails' {} a -> s {architecture = a} :: AwsEcrContainerImageDetails)

-- | The name of the repository that the image belongs to.
awsEcrContainerImageDetails_repositoryName :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_repositoryName = Lens.lens (\AwsEcrContainerImageDetails' {repositoryName} -> repositoryName) (\s@AwsEcrContainerImageDetails' {} a -> s {repositoryName = a} :: AwsEcrContainerImageDetails)

instance Core.FromJSON AwsEcrContainerImageDetails where
  parseJSON =
    Core.withObject
      "AwsEcrContainerImageDetails"
      ( \x ->
          AwsEcrContainerImageDetails'
            Prelude.<$> (x Core..:? "RegistryId")
            Prelude.<*> (x Core..:? "ImageTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ImageDigest")
            Prelude.<*> (x Core..:? "ImagePublishedAt")
            Prelude.<*> (x Core..:? "Architecture")
            Prelude.<*> (x Core..:? "RepositoryName")
      )

instance Prelude.Hashable AwsEcrContainerImageDetails

instance Prelude.NFData AwsEcrContainerImageDetails

instance Core.ToJSON AwsEcrContainerImageDetails where
  toJSON AwsEcrContainerImageDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RegistryId" Core..=) Prelude.<$> registryId,
            ("ImageTags" Core..=) Prelude.<$> imageTags,
            ("ImageDigest" Core..=) Prelude.<$> imageDigest,
            ("ImagePublishedAt" Core..=)
              Prelude.<$> imagePublishedAt,
            ("Architecture" Core..=) Prelude.<$> architecture,
            ("RepositoryName" Core..=)
              Prelude.<$> repositoryName
          ]
      )

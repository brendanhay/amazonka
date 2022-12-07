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
-- Module      : Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon ECR image.
--
-- /See:/ 'newAwsEcrContainerImageDetails' smart constructor.
data AwsEcrContainerImageDetails = AwsEcrContainerImageDetails'
  { -- | The name of the repository that the image belongs to.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The list of tags that are associated with the image.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The date and time when the image was pushed to the repository.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    imagePublishedAt :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account identifier that is associated with the
    -- registry that the image belongs to.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The sha256 digest of the image manifest.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the image. Valid values are as follows:
    --
    -- -   @arm64@
    --
    -- -   @i386@
    --
    -- -   @x86_64@
    architecture :: Prelude.Maybe Prelude.Text
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
-- 'repositoryName', 'awsEcrContainerImageDetails_repositoryName' - The name of the repository that the image belongs to.
--
-- 'imageTags', 'awsEcrContainerImageDetails_imageTags' - The list of tags that are associated with the image.
--
-- 'imagePublishedAt', 'awsEcrContainerImageDetails_imagePublishedAt' - The date and time when the image was pushed to the repository.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'registryId', 'awsEcrContainerImageDetails_registryId' - The Amazon Web Services account identifier that is associated with the
-- registry that the image belongs to.
--
-- 'imageDigest', 'awsEcrContainerImageDetails_imageDigest' - The sha256 digest of the image manifest.
--
-- 'architecture', 'awsEcrContainerImageDetails_architecture' - The architecture of the image. Valid values are as follows:
--
-- -   @arm64@
--
-- -   @i386@
--
-- -   @x86_64@
newAwsEcrContainerImageDetails ::
  AwsEcrContainerImageDetails
newAwsEcrContainerImageDetails =
  AwsEcrContainerImageDetails'
    { repositoryName =
        Prelude.Nothing,
      imageTags = Prelude.Nothing,
      imagePublishedAt = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      architecture = Prelude.Nothing
    }

-- | The name of the repository that the image belongs to.
awsEcrContainerImageDetails_repositoryName :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_repositoryName = Lens.lens (\AwsEcrContainerImageDetails' {repositoryName} -> repositoryName) (\s@AwsEcrContainerImageDetails' {} a -> s {repositoryName = a} :: AwsEcrContainerImageDetails)

-- | The list of tags that are associated with the image.
awsEcrContainerImageDetails_imageTags :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe [Prelude.Text])
awsEcrContainerImageDetails_imageTags = Lens.lens (\AwsEcrContainerImageDetails' {imageTags} -> imageTags) (\s@AwsEcrContainerImageDetails' {} a -> s {imageTags = a} :: AwsEcrContainerImageDetails) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the image was pushed to the repository.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEcrContainerImageDetails_imagePublishedAt :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_imagePublishedAt = Lens.lens (\AwsEcrContainerImageDetails' {imagePublishedAt} -> imagePublishedAt) (\s@AwsEcrContainerImageDetails' {} a -> s {imagePublishedAt = a} :: AwsEcrContainerImageDetails)

-- | The Amazon Web Services account identifier that is associated with the
-- registry that the image belongs to.
awsEcrContainerImageDetails_registryId :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_registryId = Lens.lens (\AwsEcrContainerImageDetails' {registryId} -> registryId) (\s@AwsEcrContainerImageDetails' {} a -> s {registryId = a} :: AwsEcrContainerImageDetails)

-- | The sha256 digest of the image manifest.
awsEcrContainerImageDetails_imageDigest :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_imageDigest = Lens.lens (\AwsEcrContainerImageDetails' {imageDigest} -> imageDigest) (\s@AwsEcrContainerImageDetails' {} a -> s {imageDigest = a} :: AwsEcrContainerImageDetails)

-- | The architecture of the image. Valid values are as follows:
--
-- -   @arm64@
--
-- -   @i386@
--
-- -   @x86_64@
awsEcrContainerImageDetails_architecture :: Lens.Lens' AwsEcrContainerImageDetails (Prelude.Maybe Prelude.Text)
awsEcrContainerImageDetails_architecture = Lens.lens (\AwsEcrContainerImageDetails' {architecture} -> architecture) (\s@AwsEcrContainerImageDetails' {} a -> s {architecture = a} :: AwsEcrContainerImageDetails)

instance Data.FromJSON AwsEcrContainerImageDetails where
  parseJSON =
    Data.withObject
      "AwsEcrContainerImageDetails"
      ( \x ->
          AwsEcrContainerImageDetails'
            Prelude.<$> (x Data..:? "RepositoryName")
            Prelude.<*> (x Data..:? "ImageTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ImagePublishedAt")
            Prelude.<*> (x Data..:? "RegistryId")
            Prelude.<*> (x Data..:? "ImageDigest")
            Prelude.<*> (x Data..:? "Architecture")
      )

instance Prelude.Hashable AwsEcrContainerImageDetails where
  hashWithSalt _salt AwsEcrContainerImageDetails' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` imagePublishedAt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` imageDigest
      `Prelude.hashWithSalt` architecture

instance Prelude.NFData AwsEcrContainerImageDetails where
  rnf AwsEcrContainerImageDetails' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf imagePublishedAt
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf architecture

instance Data.ToJSON AwsEcrContainerImageDetails where
  toJSON AwsEcrContainerImageDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RepositoryName" Data..=)
              Prelude.<$> repositoryName,
            ("ImageTags" Data..=) Prelude.<$> imageTags,
            ("ImagePublishedAt" Data..=)
              Prelude.<$> imagePublishedAt,
            ("RegistryId" Data..=) Prelude.<$> registryId,
            ("ImageDigest" Data..=) Prelude.<$> imageDigest,
            ("Architecture" Data..=) Prelude.<$> architecture
          ]
      )

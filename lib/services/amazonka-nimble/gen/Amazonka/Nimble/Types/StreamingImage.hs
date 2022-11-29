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
-- Module      : Amazonka.Nimble.Types.StreamingImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration
import Amazonka.Nimble.Types.StreamingImageState
import Amazonka.Nimble.Types.StreamingImageStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Represents a streaming image resource.
--
-- Streaming images are used by studio users to select which operating
-- system and software they want to use in a Nimble Studio streaming
-- session.
--
-- Amazon provides a number of streaming images that include popular
-- 3rd-party software.
--
-- You can create your own streaming images using an Amazon Elastic Compute
-- Cloud (Amazon EC2) machine image that you create for this purpose. You
-- can also include software that your users require.
--
-- /See:/ 'newStreamingImage' smart constructor.
data StreamingImage = StreamingImage'
  { -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the streaming image.
    streamingImageId :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for a streaming image resource.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of an EC2 machine image with which to create the streaming image.
    ec2ImageId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe StreamingImageState,
    -- | The owner of the streaming image, either the studioId that contains the
    -- streaming image, or \'amazon\' for images that are provided by Amazon
    -- Nimble Studio.
    owner :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the streaming image.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The platform of the streaming image, either WINDOWS or LINUX.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The list of EULAs that must be accepted before a Streaming Session can
    -- be started using this streaming image.
    eulaIds :: Prelude.Maybe [Prelude.Text],
    -- | The encryption configuration.
    encryptionConfiguration :: Prelude.Maybe StreamingImageEncryptionConfiguration,
    -- | The status code.
    statusCode :: Prelude.Maybe StreamingImageStatusCode,
    -- | The status message for the streaming image.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'streamingImage_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'streamingImageId', 'streamingImage_streamingImageId' - The ID of the streaming image.
--
-- 'name', 'streamingImage_name' - A friendly name for a streaming image resource.
--
-- 'ec2ImageId', 'streamingImage_ec2ImageId' - The ID of an EC2 machine image with which to create the streaming image.
--
-- 'arn', 'streamingImage_arn' - The ARN of the resource.
--
-- 'state', 'streamingImage_state' - The current state.
--
-- 'owner', 'streamingImage_owner' - The owner of the streaming image, either the studioId that contains the
-- streaming image, or \'amazon\' for images that are provided by Amazon
-- Nimble Studio.
--
-- 'description', 'streamingImage_description' - A human-readable description of the streaming image.
--
-- 'platform', 'streamingImage_platform' - The platform of the streaming image, either WINDOWS or LINUX.
--
-- 'eulaIds', 'streamingImage_eulaIds' - The list of EULAs that must be accepted before a Streaming Session can
-- be started using this streaming image.
--
-- 'encryptionConfiguration', 'streamingImage_encryptionConfiguration' - The encryption configuration.
--
-- 'statusCode', 'streamingImage_statusCode' - The status code.
--
-- 'statusMessage', 'streamingImage_statusMessage' - The status message for the streaming image.
newStreamingImage ::
  StreamingImage
newStreamingImage =
  StreamingImage'
    { tags = Prelude.Nothing,
      streamingImageId = Prelude.Nothing,
      name = Prelude.Nothing,
      ec2ImageId = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing,
      platform = Prelude.Nothing,
      eulaIds = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
streamingImage_tags :: Lens.Lens' StreamingImage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamingImage_tags = Lens.lens (\StreamingImage' {tags} -> tags) (\s@StreamingImage' {} a -> s {tags = a} :: StreamingImage) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the streaming image.
streamingImage_streamingImageId :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_streamingImageId = Lens.lens (\StreamingImage' {streamingImageId} -> streamingImageId) (\s@StreamingImage' {} a -> s {streamingImageId = a} :: StreamingImage)

-- | A friendly name for a streaming image resource.
streamingImage_name :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_name = Lens.lens (\StreamingImage' {name} -> name) (\s@StreamingImage' {} a -> s {name = a} :: StreamingImage) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of an EC2 machine image with which to create the streaming image.
streamingImage_ec2ImageId :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_ec2ImageId = Lens.lens (\StreamingImage' {ec2ImageId} -> ec2ImageId) (\s@StreamingImage' {} a -> s {ec2ImageId = a} :: StreamingImage)

-- | The ARN of the resource.
streamingImage_arn :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_arn = Lens.lens (\StreamingImage' {arn} -> arn) (\s@StreamingImage' {} a -> s {arn = a} :: StreamingImage)

-- | The current state.
streamingImage_state :: Lens.Lens' StreamingImage (Prelude.Maybe StreamingImageState)
streamingImage_state = Lens.lens (\StreamingImage' {state} -> state) (\s@StreamingImage' {} a -> s {state = a} :: StreamingImage)

-- | The owner of the streaming image, either the studioId that contains the
-- streaming image, or \'amazon\' for images that are provided by Amazon
-- Nimble Studio.
streamingImage_owner :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_owner = Lens.lens (\StreamingImage' {owner} -> owner) (\s@StreamingImage' {} a -> s {owner = a} :: StreamingImage)

-- | A human-readable description of the streaming image.
streamingImage_description :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_description = Lens.lens (\StreamingImage' {description} -> description) (\s@StreamingImage' {} a -> s {description = a} :: StreamingImage) Prelude.. Lens.mapping Core._Sensitive

-- | The platform of the streaming image, either WINDOWS or LINUX.
streamingImage_platform :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_platform = Lens.lens (\StreamingImage' {platform} -> platform) (\s@StreamingImage' {} a -> s {platform = a} :: StreamingImage)

-- | The list of EULAs that must be accepted before a Streaming Session can
-- be started using this streaming image.
streamingImage_eulaIds :: Lens.Lens' StreamingImage (Prelude.Maybe [Prelude.Text])
streamingImage_eulaIds = Lens.lens (\StreamingImage' {eulaIds} -> eulaIds) (\s@StreamingImage' {} a -> s {eulaIds = a} :: StreamingImage) Prelude.. Lens.mapping Lens.coerced

-- | The encryption configuration.
streamingImage_encryptionConfiguration :: Lens.Lens' StreamingImage (Prelude.Maybe StreamingImageEncryptionConfiguration)
streamingImage_encryptionConfiguration = Lens.lens (\StreamingImage' {encryptionConfiguration} -> encryptionConfiguration) (\s@StreamingImage' {} a -> s {encryptionConfiguration = a} :: StreamingImage)

-- | The status code.
streamingImage_statusCode :: Lens.Lens' StreamingImage (Prelude.Maybe StreamingImageStatusCode)
streamingImage_statusCode = Lens.lens (\StreamingImage' {statusCode} -> statusCode) (\s@StreamingImage' {} a -> s {statusCode = a} :: StreamingImage)

-- | The status message for the streaming image.
streamingImage_statusMessage :: Lens.Lens' StreamingImage (Prelude.Maybe Prelude.Text)
streamingImage_statusMessage = Lens.lens (\StreamingImage' {statusMessage} -> statusMessage) (\s@StreamingImage' {} a -> s {statusMessage = a} :: StreamingImage)

instance Core.FromJSON StreamingImage where
  parseJSON =
    Core.withObject
      "StreamingImage"
      ( \x ->
          StreamingImage'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "streamingImageId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "ec2ImageId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "eulaIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "encryptionConfiguration")
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "statusMessage")
      )

instance Prelude.Hashable StreamingImage where
  hashWithSalt _salt StreamingImage' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ec2ImageId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` eulaIds
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData StreamingImage where
  rnf StreamingImage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ec2ImageId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf eulaIds
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage

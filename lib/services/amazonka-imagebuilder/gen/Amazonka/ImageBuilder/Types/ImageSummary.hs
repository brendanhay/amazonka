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
-- Module      : Amazonka.ImageBuilder.Types.ImageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageSummary where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.ImageState
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.OutputResources
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An image summary.
--
-- /See:/ 'newImageSummary' smart constructor.
data ImageSummary = ImageSummary'
  { -- | The state of the image.
    state :: Prelude.Maybe ImageState,
    -- | The platform of the image.
    platform :: Prelude.Maybe Platform,
    -- | The Amazon Resource Name (ARN) of the image.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The owner of the image.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the image.
    version :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this is an AMI or container image.
    type' :: Prelude.Maybe ImageType,
    -- | The output resources produced when creating this image.
    outputResources :: Prelude.Maybe OutputResources,
    -- | The operating system version of the instance. For example, Amazon Linux
    -- 2, Ubuntu 18, or Microsoft Windows Server 2019.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags of the image.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'imageSummary_state' - The state of the image.
--
-- 'platform', 'imageSummary_platform' - The platform of the image.
--
-- 'arn', 'imageSummary_arn' - The Amazon Resource Name (ARN) of the image.
--
-- 'owner', 'imageSummary_owner' - The owner of the image.
--
-- 'dateCreated', 'imageSummary_dateCreated' - The date on which this image was created.
--
-- 'name', 'imageSummary_name' - The name of the image.
--
-- 'version', 'imageSummary_version' - The version of the image.
--
-- 'type'', 'imageSummary_type' - Specifies whether this is an AMI or container image.
--
-- 'outputResources', 'imageSummary_outputResources' - The output resources produced when creating this image.
--
-- 'osVersion', 'imageSummary_osVersion' - The operating system version of the instance. For example, Amazon Linux
-- 2, Ubuntu 18, or Microsoft Windows Server 2019.
--
-- 'tags', 'imageSummary_tags' - The tags of the image.
newImageSummary ::
  ImageSummary
newImageSummary =
  ImageSummary'
    { state = Prelude.Nothing,
      platform = Prelude.Nothing,
      arn = Prelude.Nothing,
      owner = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      type' = Prelude.Nothing,
      outputResources = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the image.
imageSummary_state :: Lens.Lens' ImageSummary (Prelude.Maybe ImageState)
imageSummary_state = Lens.lens (\ImageSummary' {state} -> state) (\s@ImageSummary' {} a -> s {state = a} :: ImageSummary)

-- | The platform of the image.
imageSummary_platform :: Lens.Lens' ImageSummary (Prelude.Maybe Platform)
imageSummary_platform = Lens.lens (\ImageSummary' {platform} -> platform) (\s@ImageSummary' {} a -> s {platform = a} :: ImageSummary)

-- | The Amazon Resource Name (ARN) of the image.
imageSummary_arn :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_arn = Lens.lens (\ImageSummary' {arn} -> arn) (\s@ImageSummary' {} a -> s {arn = a} :: ImageSummary)

-- | The owner of the image.
imageSummary_owner :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_owner = Lens.lens (\ImageSummary' {owner} -> owner) (\s@ImageSummary' {} a -> s {owner = a} :: ImageSummary)

-- | The date on which this image was created.
imageSummary_dateCreated :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_dateCreated = Lens.lens (\ImageSummary' {dateCreated} -> dateCreated) (\s@ImageSummary' {} a -> s {dateCreated = a} :: ImageSummary)

-- | The name of the image.
imageSummary_name :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_name = Lens.lens (\ImageSummary' {name} -> name) (\s@ImageSummary' {} a -> s {name = a} :: ImageSummary)

-- | The version of the image.
imageSummary_version :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_version = Lens.lens (\ImageSummary' {version} -> version) (\s@ImageSummary' {} a -> s {version = a} :: ImageSummary)

-- | Specifies whether this is an AMI or container image.
imageSummary_type :: Lens.Lens' ImageSummary (Prelude.Maybe ImageType)
imageSummary_type = Lens.lens (\ImageSummary' {type'} -> type') (\s@ImageSummary' {} a -> s {type' = a} :: ImageSummary)

-- | The output resources produced when creating this image.
imageSummary_outputResources :: Lens.Lens' ImageSummary (Prelude.Maybe OutputResources)
imageSummary_outputResources = Lens.lens (\ImageSummary' {outputResources} -> outputResources) (\s@ImageSummary' {} a -> s {outputResources = a} :: ImageSummary)

-- | The operating system version of the instance. For example, Amazon Linux
-- 2, Ubuntu 18, or Microsoft Windows Server 2019.
imageSummary_osVersion :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_osVersion = Lens.lens (\ImageSummary' {osVersion} -> osVersion) (\s@ImageSummary' {} a -> s {osVersion = a} :: ImageSummary)

-- | The tags of the image.
imageSummary_tags :: Lens.Lens' ImageSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageSummary_tags = Lens.lens (\ImageSummary' {tags} -> tags) (\s@ImageSummary' {} a -> s {tags = a} :: ImageSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ImageSummary where
  parseJSON =
    Core.withObject
      "ImageSummary"
      ( \x ->
          ImageSummary'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "outputResources")
            Prelude.<*> (x Core..:? "osVersion")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ImageSummary where
  hashWithSalt salt' ImageSummary' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` outputResources
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` state

instance Prelude.NFData ImageSummary where
  rnf ImageSummary' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf outputResources
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf platform

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
-- Module      : Amazonka.ImageBuilder.Types.ImageRecipeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageRecipeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | A summary of an image recipe.
--
-- /See:/ 'newImageRecipeSummary' smart constructor.
data ImageRecipeSummary = ImageRecipeSummary'
  { -- | The Amazon Resource Name (ARN) of the image recipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image recipe was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the image recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the image recipe.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The base image of the image recipe.
    parentImage :: Prelude.Maybe Prelude.Text,
    -- | The platform of the image recipe.
    platform :: Prelude.Maybe Platform,
    -- | The tags of the image recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageRecipeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'imageRecipeSummary_arn' - The Amazon Resource Name (ARN) of the image recipe.
--
-- 'dateCreated', 'imageRecipeSummary_dateCreated' - The date on which this image recipe was created.
--
-- 'name', 'imageRecipeSummary_name' - The name of the image recipe.
--
-- 'owner', 'imageRecipeSummary_owner' - The owner of the image recipe.
--
-- 'parentImage', 'imageRecipeSummary_parentImage' - The base image of the image recipe.
--
-- 'platform', 'imageRecipeSummary_platform' - The platform of the image recipe.
--
-- 'tags', 'imageRecipeSummary_tags' - The tags of the image recipe.
newImageRecipeSummary ::
  ImageRecipeSummary
newImageRecipeSummary =
  ImageRecipeSummary'
    { arn = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      parentImage = Prelude.Nothing,
      platform = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the image recipe.
imageRecipeSummary_arn :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Prelude.Text)
imageRecipeSummary_arn = Lens.lens (\ImageRecipeSummary' {arn} -> arn) (\s@ImageRecipeSummary' {} a -> s {arn = a} :: ImageRecipeSummary)

-- | The date on which this image recipe was created.
imageRecipeSummary_dateCreated :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Prelude.Text)
imageRecipeSummary_dateCreated = Lens.lens (\ImageRecipeSummary' {dateCreated} -> dateCreated) (\s@ImageRecipeSummary' {} a -> s {dateCreated = a} :: ImageRecipeSummary)

-- | The name of the image recipe.
imageRecipeSummary_name :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Prelude.Text)
imageRecipeSummary_name = Lens.lens (\ImageRecipeSummary' {name} -> name) (\s@ImageRecipeSummary' {} a -> s {name = a} :: ImageRecipeSummary)

-- | The owner of the image recipe.
imageRecipeSummary_owner :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Prelude.Text)
imageRecipeSummary_owner = Lens.lens (\ImageRecipeSummary' {owner} -> owner) (\s@ImageRecipeSummary' {} a -> s {owner = a} :: ImageRecipeSummary)

-- | The base image of the image recipe.
imageRecipeSummary_parentImage :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Prelude.Text)
imageRecipeSummary_parentImage = Lens.lens (\ImageRecipeSummary' {parentImage} -> parentImage) (\s@ImageRecipeSummary' {} a -> s {parentImage = a} :: ImageRecipeSummary)

-- | The platform of the image recipe.
imageRecipeSummary_platform :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe Platform)
imageRecipeSummary_platform = Lens.lens (\ImageRecipeSummary' {platform} -> platform) (\s@ImageRecipeSummary' {} a -> s {platform = a} :: ImageRecipeSummary)

-- | The tags of the image recipe.
imageRecipeSummary_tags :: Lens.Lens' ImageRecipeSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageRecipeSummary_tags = Lens.lens (\ImageRecipeSummary' {tags} -> tags) (\s@ImageRecipeSummary' {} a -> s {tags = a} :: ImageRecipeSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ImageRecipeSummary where
  parseJSON =
    Data.withObject
      "ImageRecipeSummary"
      ( \x ->
          ImageRecipeSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "parentImage")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ImageRecipeSummary where
  hashWithSalt _salt ImageRecipeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ImageRecipeSummary where
  rnf ImageRecipeSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf parentImage
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf tags

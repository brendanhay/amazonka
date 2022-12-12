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
-- Module      : Amazonka.ImageBuilder.Types.ContainerRecipeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ContainerRecipeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ContainerType
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | A summary of a container recipe
--
-- /See:/ 'newContainerRecipeSummary' smart constructor.
data ContainerRecipeSummary = ContainerRecipeSummary'
  { -- | The Amazon Resource Name (ARN) of the container recipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of container, such as \"Docker\".
    containerType :: Prelude.Maybe ContainerType,
    -- | The date when this container recipe was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the container recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the container recipe.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The base image for the container recipe.
    parentImage :: Prelude.Maybe Prelude.Text,
    -- | The system platform for the container, such as Windows or Linux.
    platform :: Prelude.Maybe Platform,
    -- | Tags that are attached to the container recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerRecipeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'containerRecipeSummary_arn' - The Amazon Resource Name (ARN) of the container recipe.
--
-- 'containerType', 'containerRecipeSummary_containerType' - Specifies the type of container, such as \"Docker\".
--
-- 'dateCreated', 'containerRecipeSummary_dateCreated' - The date when this container recipe was created.
--
-- 'name', 'containerRecipeSummary_name' - The name of the container recipe.
--
-- 'owner', 'containerRecipeSummary_owner' - The owner of the container recipe.
--
-- 'parentImage', 'containerRecipeSummary_parentImage' - The base image for the container recipe.
--
-- 'platform', 'containerRecipeSummary_platform' - The system platform for the container, such as Windows or Linux.
--
-- 'tags', 'containerRecipeSummary_tags' - Tags that are attached to the container recipe.
newContainerRecipeSummary ::
  ContainerRecipeSummary
newContainerRecipeSummary =
  ContainerRecipeSummary'
    { arn = Prelude.Nothing,
      containerType = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      parentImage = Prelude.Nothing,
      platform = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container recipe.
containerRecipeSummary_arn :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Prelude.Text)
containerRecipeSummary_arn = Lens.lens (\ContainerRecipeSummary' {arn} -> arn) (\s@ContainerRecipeSummary' {} a -> s {arn = a} :: ContainerRecipeSummary)

-- | Specifies the type of container, such as \"Docker\".
containerRecipeSummary_containerType :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe ContainerType)
containerRecipeSummary_containerType = Lens.lens (\ContainerRecipeSummary' {containerType} -> containerType) (\s@ContainerRecipeSummary' {} a -> s {containerType = a} :: ContainerRecipeSummary)

-- | The date when this container recipe was created.
containerRecipeSummary_dateCreated :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Prelude.Text)
containerRecipeSummary_dateCreated = Lens.lens (\ContainerRecipeSummary' {dateCreated} -> dateCreated) (\s@ContainerRecipeSummary' {} a -> s {dateCreated = a} :: ContainerRecipeSummary)

-- | The name of the container recipe.
containerRecipeSummary_name :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Prelude.Text)
containerRecipeSummary_name = Lens.lens (\ContainerRecipeSummary' {name} -> name) (\s@ContainerRecipeSummary' {} a -> s {name = a} :: ContainerRecipeSummary)

-- | The owner of the container recipe.
containerRecipeSummary_owner :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Prelude.Text)
containerRecipeSummary_owner = Lens.lens (\ContainerRecipeSummary' {owner} -> owner) (\s@ContainerRecipeSummary' {} a -> s {owner = a} :: ContainerRecipeSummary)

-- | The base image for the container recipe.
containerRecipeSummary_parentImage :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Prelude.Text)
containerRecipeSummary_parentImage = Lens.lens (\ContainerRecipeSummary' {parentImage} -> parentImage) (\s@ContainerRecipeSummary' {} a -> s {parentImage = a} :: ContainerRecipeSummary)

-- | The system platform for the container, such as Windows or Linux.
containerRecipeSummary_platform :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe Platform)
containerRecipeSummary_platform = Lens.lens (\ContainerRecipeSummary' {platform} -> platform) (\s@ContainerRecipeSummary' {} a -> s {platform = a} :: ContainerRecipeSummary)

-- | Tags that are attached to the container recipe.
containerRecipeSummary_tags :: Lens.Lens' ContainerRecipeSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
containerRecipeSummary_tags = Lens.lens (\ContainerRecipeSummary' {tags} -> tags) (\s@ContainerRecipeSummary' {} a -> s {tags = a} :: ContainerRecipeSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContainerRecipeSummary where
  parseJSON =
    Data.withObject
      "ContainerRecipeSummary"
      ( \x ->
          ContainerRecipeSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "containerType")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "parentImage")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContainerRecipeSummary where
  hashWithSalt _salt ContainerRecipeSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` containerType
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ContainerRecipeSummary where
  rnf ContainerRecipeSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf containerType
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf parentImage
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf tags

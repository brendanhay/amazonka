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
-- Module      : Amazonka.ImageBuilder.Types.ContainerDistributionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ContainerDistributionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.TargetContainerRepository
import qualified Amazonka.Prelude as Prelude

-- | Container distribution settings for encryption, licensing, and sharing
-- in a specific Region.
--
-- /See:/ 'newContainerDistributionConfiguration' smart constructor.
data ContainerDistributionConfiguration = ContainerDistributionConfiguration'
  { -- | Tags that are attached to the container distribution configuration.
    containerTags :: Prelude.Maybe [Prelude.Text],
    -- | The description of the container distribution configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The destination repository for the container distribution configuration.
    targetRepository :: TargetContainerRepository
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerTags', 'containerDistributionConfiguration_containerTags' - Tags that are attached to the container distribution configuration.
--
-- 'description', 'containerDistributionConfiguration_description' - The description of the container distribution configuration.
--
-- 'targetRepository', 'containerDistributionConfiguration_targetRepository' - The destination repository for the container distribution configuration.
newContainerDistributionConfiguration ::
  -- | 'targetRepository'
  TargetContainerRepository ->
  ContainerDistributionConfiguration
newContainerDistributionConfiguration
  pTargetRepository_ =
    ContainerDistributionConfiguration'
      { containerTags =
          Prelude.Nothing,
        description = Prelude.Nothing,
        targetRepository = pTargetRepository_
      }

-- | Tags that are attached to the container distribution configuration.
containerDistributionConfiguration_containerTags :: Lens.Lens' ContainerDistributionConfiguration (Prelude.Maybe [Prelude.Text])
containerDistributionConfiguration_containerTags = Lens.lens (\ContainerDistributionConfiguration' {containerTags} -> containerTags) (\s@ContainerDistributionConfiguration' {} a -> s {containerTags = a} :: ContainerDistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The description of the container distribution configuration.
containerDistributionConfiguration_description :: Lens.Lens' ContainerDistributionConfiguration (Prelude.Maybe Prelude.Text)
containerDistributionConfiguration_description = Lens.lens (\ContainerDistributionConfiguration' {description} -> description) (\s@ContainerDistributionConfiguration' {} a -> s {description = a} :: ContainerDistributionConfiguration)

-- | The destination repository for the container distribution configuration.
containerDistributionConfiguration_targetRepository :: Lens.Lens' ContainerDistributionConfiguration TargetContainerRepository
containerDistributionConfiguration_targetRepository = Lens.lens (\ContainerDistributionConfiguration' {targetRepository} -> targetRepository) (\s@ContainerDistributionConfiguration' {} a -> s {targetRepository = a} :: ContainerDistributionConfiguration)

instance
  Data.FromJSON
    ContainerDistributionConfiguration
  where
  parseJSON =
    Data.withObject
      "ContainerDistributionConfiguration"
      ( \x ->
          ContainerDistributionConfiguration'
            Prelude.<$> (x Data..:? "containerTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "targetRepository")
      )

instance
  Prelude.Hashable
    ContainerDistributionConfiguration
  where
  hashWithSalt
    _salt
    ContainerDistributionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` containerTags
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` targetRepository

instance
  Prelude.NFData
    ContainerDistributionConfiguration
  where
  rnf ContainerDistributionConfiguration' {..} =
    Prelude.rnf containerTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf targetRepository

instance
  Data.ToJSON
    ContainerDistributionConfiguration
  where
  toJSON ContainerDistributionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerTags" Data..=) Prelude.<$> containerTags,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("targetRepository" Data..= targetRepository)
          ]
      )

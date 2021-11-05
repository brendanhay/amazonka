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
-- Module      : Network.AWS.ImageBuilder.Types.ContainerDistributionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImageBuilder.Types.ContainerDistributionConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types.TargetContainerRepository
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  Core.FromJSON
    ContainerDistributionConfiguration
  where
  parseJSON =
    Core.withObject
      "ContainerDistributionConfiguration"
      ( \x ->
          ContainerDistributionConfiguration'
            Prelude.<$> (x Core..:? "containerTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "targetRepository")
      )

instance
  Prelude.Hashable
    ContainerDistributionConfiguration

instance
  Prelude.NFData
    ContainerDistributionConfiguration

instance
  Core.ToJSON
    ContainerDistributionConfiguration
  where
  toJSON ContainerDistributionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("containerTags" Core..=) Prelude.<$> containerTags,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just
              ("targetRepository" Core..= targetRepository)
          ]
      )

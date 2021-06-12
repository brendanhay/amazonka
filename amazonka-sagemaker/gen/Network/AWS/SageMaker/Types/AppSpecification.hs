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
-- Module      : Network.AWS.SageMaker.Types.AppSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration to run a processing job in a specified container image.
--
-- /See:/ 'newAppSpecification' smart constructor.
data AppSpecification = AppSpecification'
  { -- | The arguments for a container used to run a processing job.
    containerArguments :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The entrypoint for a container used to run a processing job.
    containerEntrypoint :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The container image to be run by the processing job.
    imageUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AppSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerArguments', 'appSpecification_containerArguments' - The arguments for a container used to run a processing job.
--
-- 'containerEntrypoint', 'appSpecification_containerEntrypoint' - The entrypoint for a container used to run a processing job.
--
-- 'imageUri', 'appSpecification_imageUri' - The container image to be run by the processing job.
newAppSpecification ::
  -- | 'imageUri'
  Core.Text ->
  AppSpecification
newAppSpecification pImageUri_ =
  AppSpecification'
    { containerArguments =
        Core.Nothing,
      containerEntrypoint = Core.Nothing,
      imageUri = pImageUri_
    }

-- | The arguments for a container used to run a processing job.
appSpecification_containerArguments :: Lens.Lens' AppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
appSpecification_containerArguments = Lens.lens (\AppSpecification' {containerArguments} -> containerArguments) (\s@AppSpecification' {} a -> s {containerArguments = a} :: AppSpecification) Core.. Lens.mapping Lens._Coerce

-- | The entrypoint for a container used to run a processing job.
appSpecification_containerEntrypoint :: Lens.Lens' AppSpecification (Core.Maybe (Core.NonEmpty Core.Text))
appSpecification_containerEntrypoint = Lens.lens (\AppSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@AppSpecification' {} a -> s {containerEntrypoint = a} :: AppSpecification) Core.. Lens.mapping Lens._Coerce

-- | The container image to be run by the processing job.
appSpecification_imageUri :: Lens.Lens' AppSpecification Core.Text
appSpecification_imageUri = Lens.lens (\AppSpecification' {imageUri} -> imageUri) (\s@AppSpecification' {} a -> s {imageUri = a} :: AppSpecification)

instance Core.FromJSON AppSpecification where
  parseJSON =
    Core.withObject
      "AppSpecification"
      ( \x ->
          AppSpecification'
            Core.<$> (x Core..:? "ContainerArguments")
            Core.<*> (x Core..:? "ContainerEntrypoint")
            Core.<*> (x Core..: "ImageUri")
      )

instance Core.Hashable AppSpecification

instance Core.NFData AppSpecification

instance Core.ToJSON AppSpecification where
  toJSON AppSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContainerArguments" Core..=)
              Core.<$> containerArguments,
            ("ContainerEntrypoint" Core..=)
              Core.<$> containerEntrypoint,
            Core.Just ("ImageUri" Core..= imageUri)
          ]
      )

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
-- Module      : Network.AWS.MediaConvert.Types.AvailBlanking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvailBlanking where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for Avail Blanking
--
-- /See:/ 'newAvailBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { -- | Blanking image to be used. Leave empty for solid black. Only bmp and png
    -- images are supported.
    availBlankingImage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailBlanking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availBlankingImage', 'availBlanking_availBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
newAvailBlanking ::
  AvailBlanking
newAvailBlanking =
  AvailBlanking' {availBlankingImage = Core.Nothing}

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
availBlanking_availBlankingImage :: Lens.Lens' AvailBlanking (Core.Maybe Core.Text)
availBlanking_availBlankingImage = Lens.lens (\AvailBlanking' {availBlankingImage} -> availBlankingImage) (\s@AvailBlanking' {} a -> s {availBlankingImage = a} :: AvailBlanking)

instance Core.FromJSON AvailBlanking where
  parseJSON =
    Core.withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            Core.<$> (x Core..:? "availBlankingImage")
      )

instance Core.Hashable AvailBlanking

instance Core.NFData AvailBlanking

instance Core.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Core.object
      ( Core.catMaybes
          [ ("availBlankingImage" Core..=)
              Core.<$> availBlankingImage
          ]
      )

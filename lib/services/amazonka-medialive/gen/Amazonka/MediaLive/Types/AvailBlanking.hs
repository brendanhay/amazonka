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
-- Module      : Amazonka.MediaLive.Types.AvailBlanking
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AvailBlanking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.AvailBlankingState
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Avail Blanking
--
-- /See:/ 'newAvailBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { -- | When set to enabled, causes video, audio and captions to be blanked when
    -- insertion metadata is added.
    state :: Prelude.Maybe AvailBlankingState,
    -- | Blanking image to be used. Leave empty for solid black. Only bmp and png
    -- images are supported.
    availBlankingImage :: Prelude.Maybe InputLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailBlanking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'availBlanking_state' - When set to enabled, causes video, audio and captions to be blanked when
-- insertion metadata is added.
--
-- 'availBlankingImage', 'availBlanking_availBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
newAvailBlanking ::
  AvailBlanking
newAvailBlanking =
  AvailBlanking'
    { state = Prelude.Nothing,
      availBlankingImage = Prelude.Nothing
    }

-- | When set to enabled, causes video, audio and captions to be blanked when
-- insertion metadata is added.
availBlanking_state :: Lens.Lens' AvailBlanking (Prelude.Maybe AvailBlankingState)
availBlanking_state = Lens.lens (\AvailBlanking' {state} -> state) (\s@AvailBlanking' {} a -> s {state = a} :: AvailBlanking)

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
availBlanking_availBlankingImage :: Lens.Lens' AvailBlanking (Prelude.Maybe InputLocation)
availBlanking_availBlankingImage = Lens.lens (\AvailBlanking' {availBlankingImage} -> availBlankingImage) (\s@AvailBlanking' {} a -> s {availBlankingImage = a} :: AvailBlanking)

instance Core.FromJSON AvailBlanking where
  parseJSON =
    Core.withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "availBlankingImage")
      )

instance Prelude.Hashable AvailBlanking where
  hashWithSalt _salt AvailBlanking' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` availBlankingImage

instance Prelude.NFData AvailBlanking where
  rnf AvailBlanking' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf availBlankingImage

instance Core.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("state" Core..=) Prelude.<$> state,
            ("availBlankingImage" Core..=)
              Prelude.<$> availBlankingImage
          ]
      )

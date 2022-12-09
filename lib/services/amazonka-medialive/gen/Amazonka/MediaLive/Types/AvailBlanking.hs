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
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AvailBlankingState
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Avail Blanking
--
-- /See:/ 'newAvailBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { -- | Blanking image to be used. Leave empty for solid black. Only bmp and png
    -- images are supported.
    availBlankingImage :: Prelude.Maybe InputLocation,
    -- | When set to enabled, causes video, audio and captions to be blanked when
    -- insertion metadata is added.
    state :: Prelude.Maybe AvailBlankingState
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
-- 'availBlankingImage', 'availBlanking_availBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
--
-- 'state', 'availBlanking_state' - When set to enabled, causes video, audio and captions to be blanked when
-- insertion metadata is added.
newAvailBlanking ::
  AvailBlanking
newAvailBlanking =
  AvailBlanking'
    { availBlankingImage =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png
-- images are supported.
availBlanking_availBlankingImage :: Lens.Lens' AvailBlanking (Prelude.Maybe InputLocation)
availBlanking_availBlankingImage = Lens.lens (\AvailBlanking' {availBlankingImage} -> availBlankingImage) (\s@AvailBlanking' {} a -> s {availBlankingImage = a} :: AvailBlanking)

-- | When set to enabled, causes video, audio and captions to be blanked when
-- insertion metadata is added.
availBlanking_state :: Lens.Lens' AvailBlanking (Prelude.Maybe AvailBlankingState)
availBlanking_state = Lens.lens (\AvailBlanking' {state} -> state) (\s@AvailBlanking' {} a -> s {state = a} :: AvailBlanking)

instance Data.FromJSON AvailBlanking where
  parseJSON =
    Data.withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            Prelude.<$> (x Data..:? "availBlankingImage")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable AvailBlanking where
  hashWithSalt _salt AvailBlanking' {..} =
    _salt `Prelude.hashWithSalt` availBlankingImage
      `Prelude.hashWithSalt` state

instance Prelude.NFData AvailBlanking where
  rnf AvailBlanking' {..} =
    Prelude.rnf availBlankingImage
      `Prelude.seq` Prelude.rnf state

instance Data.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("availBlankingImage" Data..=)
              Prelude.<$> availBlankingImage,
            ("state" Data..=) Prelude.<$> state
          ]
      )

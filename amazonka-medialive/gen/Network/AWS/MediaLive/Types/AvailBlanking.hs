{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.AvailBlanking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlanking where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AvailBlankingState
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AvailBlanking where
  parseJSON =
    Prelude.withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            Prelude.<$> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "availBlankingImage")
      )

instance Prelude.Hashable AvailBlanking

instance Prelude.NFData AvailBlanking

instance Prelude.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("state" Prelude..=) Prelude.<$> state,
            ("availBlankingImage" Prelude..=)
              Prelude.<$> availBlankingImage
          ]
      )

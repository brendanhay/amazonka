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
-- Module      : Network.AWS.MediaLive.Types.InputLossFailoverSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossFailoverSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
--
-- /See:/ 'newInputLossFailoverSettings' smart constructor.
data InputLossFailoverSettings = InputLossFailoverSettings'
  { -- | The amount of time (in milliseconds) that no input is detected. After
    -- that time, an input failover will occur.
    inputLossThresholdMsec :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputLossFailoverSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLossThresholdMsec', 'inputLossFailoverSettings_inputLossThresholdMsec' - The amount of time (in milliseconds) that no input is detected. After
-- that time, an input failover will occur.
newInputLossFailoverSettings ::
  InputLossFailoverSettings
newInputLossFailoverSettings =
  InputLossFailoverSettings'
    { inputLossThresholdMsec =
        Core.Nothing
    }

-- | The amount of time (in milliseconds) that no input is detected. After
-- that time, an input failover will occur.
inputLossFailoverSettings_inputLossThresholdMsec :: Lens.Lens' InputLossFailoverSettings (Core.Maybe Core.Natural)
inputLossFailoverSettings_inputLossThresholdMsec = Lens.lens (\InputLossFailoverSettings' {inputLossThresholdMsec} -> inputLossThresholdMsec) (\s@InputLossFailoverSettings' {} a -> s {inputLossThresholdMsec = a} :: InputLossFailoverSettings)

instance Core.FromJSON InputLossFailoverSettings where
  parseJSON =
    Core.withObject
      "InputLossFailoverSettings"
      ( \x ->
          InputLossFailoverSettings'
            Core.<$> (x Core..:? "inputLossThresholdMsec")
      )

instance Core.Hashable InputLossFailoverSettings

instance Core.NFData InputLossFailoverSettings

instance Core.ToJSON InputLossFailoverSettings where
  toJSON InputLossFailoverSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inputLossThresholdMsec" Core..=)
              Core.<$> inputLossThresholdMsec
          ]
      )

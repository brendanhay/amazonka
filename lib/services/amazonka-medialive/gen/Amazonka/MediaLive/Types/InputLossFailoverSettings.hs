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
-- Module      : Amazonka.MediaLive.Types.InputLossFailoverSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputLossFailoverSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | MediaLive will perform a failover if content is not detected in this
-- input for the specified period.
--
-- /See:/ 'newInputLossFailoverSettings' smart constructor.
data InputLossFailoverSettings = InputLossFailoverSettings'
  { -- | The amount of time (in milliseconds) that no input is detected. After
    -- that time, an input failover will occur.
    inputLossThresholdMsec :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The amount of time (in milliseconds) that no input is detected. After
-- that time, an input failover will occur.
inputLossFailoverSettings_inputLossThresholdMsec :: Lens.Lens' InputLossFailoverSettings (Prelude.Maybe Prelude.Natural)
inputLossFailoverSettings_inputLossThresholdMsec = Lens.lens (\InputLossFailoverSettings' {inputLossThresholdMsec} -> inputLossThresholdMsec) (\s@InputLossFailoverSettings' {} a -> s {inputLossThresholdMsec = a} :: InputLossFailoverSettings)

instance Core.FromJSON InputLossFailoverSettings where
  parseJSON =
    Core.withObject
      "InputLossFailoverSettings"
      ( \x ->
          InputLossFailoverSettings'
            Prelude.<$> (x Core..:? "inputLossThresholdMsec")
      )

instance Prelude.Hashable InputLossFailoverSettings where
  hashWithSalt _salt InputLossFailoverSettings' {..} =
    _salt `Prelude.hashWithSalt` inputLossThresholdMsec

instance Prelude.NFData InputLossFailoverSettings where
  rnf InputLossFailoverSettings' {..} =
    Prelude.rnf inputLossThresholdMsec

instance Core.ToJSON InputLossFailoverSettings where
  toJSON InputLossFailoverSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("inputLossThresholdMsec" Core..=)
              Prelude.<$> inputLossThresholdMsec
          ]
      )

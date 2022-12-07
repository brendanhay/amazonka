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
-- Module      : Amazonka.AlexaBusiness.Types.MeetingSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.MeetingSetting where

import Amazonka.AlexaBusiness.Types.RequirePin
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values that indicate whether a pin is always required (YES), never
-- required (NO), or OPTIONAL.
--
-- -   If YES, Alexa will always ask for a meeting pin.
--
-- -   If NO, Alexa will never ask for a meeting pin.
--
-- -   If OPTIONAL, Alexa will ask if you have a meeting pin and if the
--     customer responds with yes, it will ask for the meeting pin.
--
-- /See:/ 'newMeetingSetting' smart constructor.
data MeetingSetting = MeetingSetting'
  { -- | The values that indicate whether the pin is always required.
    requirePin :: RequirePin
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeetingSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requirePin', 'meetingSetting_requirePin' - The values that indicate whether the pin is always required.
newMeetingSetting ::
  -- | 'requirePin'
  RequirePin ->
  MeetingSetting
newMeetingSetting pRequirePin_ =
  MeetingSetting' {requirePin = pRequirePin_}

-- | The values that indicate whether the pin is always required.
meetingSetting_requirePin :: Lens.Lens' MeetingSetting RequirePin
meetingSetting_requirePin = Lens.lens (\MeetingSetting' {requirePin} -> requirePin) (\s@MeetingSetting' {} a -> s {requirePin = a} :: MeetingSetting)

instance Data.FromJSON MeetingSetting where
  parseJSON =
    Data.withObject
      "MeetingSetting"
      ( \x ->
          MeetingSetting' Prelude.<$> (x Data..: "RequirePin")
      )

instance Prelude.Hashable MeetingSetting where
  hashWithSalt _salt MeetingSetting' {..} =
    _salt `Prelude.hashWithSalt` requirePin

instance Prelude.NFData MeetingSetting where
  rnf MeetingSetting' {..} = Prelude.rnf requirePin

instance Data.ToJSON MeetingSetting where
  toJSON MeetingSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RequirePin" Data..= requirePin)]
      )

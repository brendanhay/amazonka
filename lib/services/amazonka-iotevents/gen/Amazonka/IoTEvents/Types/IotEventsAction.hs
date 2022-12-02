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
-- Module      : Amazonka.IoTEvents.Types.IotEventsAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.IotEventsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Sends an AWS IoT Events input, passing in information about the detector
-- model instance and the event that triggered the action.
--
-- /See:/ 'newIotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'
  { -- | You can configure the action payload when you send a message to an AWS
    -- IoT Events input.
    payload :: Prelude.Maybe Payload,
    -- | The name of the AWS IoT Events input where the data is sent.
    inputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotEventsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'iotEventsAction_payload' - You can configure the action payload when you send a message to an AWS
-- IoT Events input.
--
-- 'inputName', 'iotEventsAction_inputName' - The name of the AWS IoT Events input where the data is sent.
newIotEventsAction ::
  -- | 'inputName'
  Prelude.Text ->
  IotEventsAction
newIotEventsAction pInputName_ =
  IotEventsAction'
    { payload = Prelude.Nothing,
      inputName = pInputName_
    }

-- | You can configure the action payload when you send a message to an AWS
-- IoT Events input.
iotEventsAction_payload :: Lens.Lens' IotEventsAction (Prelude.Maybe Payload)
iotEventsAction_payload = Lens.lens (\IotEventsAction' {payload} -> payload) (\s@IotEventsAction' {} a -> s {payload = a} :: IotEventsAction)

-- | The name of the AWS IoT Events input where the data is sent.
iotEventsAction_inputName :: Lens.Lens' IotEventsAction Prelude.Text
iotEventsAction_inputName = Lens.lens (\IotEventsAction' {inputName} -> inputName) (\s@IotEventsAction' {} a -> s {inputName = a} :: IotEventsAction)

instance Data.FromJSON IotEventsAction where
  parseJSON =
    Data.withObject
      "IotEventsAction"
      ( \x ->
          IotEventsAction'
            Prelude.<$> (x Data..:? "payload")
            Prelude.<*> (x Data..: "inputName")
      )

instance Prelude.Hashable IotEventsAction where
  hashWithSalt _salt IotEventsAction' {..} =
    _salt `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` inputName

instance Prelude.NFData IotEventsAction where
  rnf IotEventsAction' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf inputName

instance Data.ToJSON IotEventsAction where
  toJSON IotEventsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("payload" Data..=) Prelude.<$> payload,
            Prelude.Just ("inputName" Data..= inputName)
          ]
      )

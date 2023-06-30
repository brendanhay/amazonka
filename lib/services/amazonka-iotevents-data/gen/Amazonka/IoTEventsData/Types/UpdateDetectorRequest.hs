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
-- Module      : Amazonka.IoTEventsData.Types.UpdateDetectorRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.UpdateDetectorRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.DetectorStateDefinition
import qualified Amazonka.Prelude as Prelude

-- | Information used to update the detector (instance).
--
-- /See:/ 'newUpdateDetectorRequest' smart constructor.
data UpdateDetectorRequest = UpdateDetectorRequest'
  { -- | The value of the input key attribute (identifying the device or system)
    -- that caused the creation of this detector (instance).
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The ID to assign to the detector update @\"message\"@. Each
    -- @\"messageId\"@ must be unique within each batch sent.
    messageId :: Prelude.Text,
    -- | The name of the detector model that created the detectors (instances).
    detectorModelName :: Prelude.Text,
    -- | The new state, variable values, and timer settings of the detector
    -- (instance).
    state :: DetectorStateDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'updateDetectorRequest_keyValue' - The value of the input key attribute (identifying the device or system)
-- that caused the creation of this detector (instance).
--
-- 'messageId', 'updateDetectorRequest_messageId' - The ID to assign to the detector update @\"message\"@. Each
-- @\"messageId\"@ must be unique within each batch sent.
--
-- 'detectorModelName', 'updateDetectorRequest_detectorModelName' - The name of the detector model that created the detectors (instances).
--
-- 'state', 'updateDetectorRequest_state' - The new state, variable values, and timer settings of the detector
-- (instance).
newUpdateDetectorRequest ::
  -- | 'messageId'
  Prelude.Text ->
  -- | 'detectorModelName'
  Prelude.Text ->
  -- | 'state'
  DetectorStateDefinition ->
  UpdateDetectorRequest
newUpdateDetectorRequest
  pMessageId_
  pDetectorModelName_
  pState_ =
    UpdateDetectorRequest'
      { keyValue = Prelude.Nothing,
        messageId = pMessageId_,
        detectorModelName = pDetectorModelName_,
        state = pState_
      }

-- | The value of the input key attribute (identifying the device or system)
-- that caused the creation of this detector (instance).
updateDetectorRequest_keyValue :: Lens.Lens' UpdateDetectorRequest (Prelude.Maybe Prelude.Text)
updateDetectorRequest_keyValue = Lens.lens (\UpdateDetectorRequest' {keyValue} -> keyValue) (\s@UpdateDetectorRequest' {} a -> s {keyValue = a} :: UpdateDetectorRequest)

-- | The ID to assign to the detector update @\"message\"@. Each
-- @\"messageId\"@ must be unique within each batch sent.
updateDetectorRequest_messageId :: Lens.Lens' UpdateDetectorRequest Prelude.Text
updateDetectorRequest_messageId = Lens.lens (\UpdateDetectorRequest' {messageId} -> messageId) (\s@UpdateDetectorRequest' {} a -> s {messageId = a} :: UpdateDetectorRequest)

-- | The name of the detector model that created the detectors (instances).
updateDetectorRequest_detectorModelName :: Lens.Lens' UpdateDetectorRequest Prelude.Text
updateDetectorRequest_detectorModelName = Lens.lens (\UpdateDetectorRequest' {detectorModelName} -> detectorModelName) (\s@UpdateDetectorRequest' {} a -> s {detectorModelName = a} :: UpdateDetectorRequest)

-- | The new state, variable values, and timer settings of the detector
-- (instance).
updateDetectorRequest_state :: Lens.Lens' UpdateDetectorRequest DetectorStateDefinition
updateDetectorRequest_state = Lens.lens (\UpdateDetectorRequest' {state} -> state) (\s@UpdateDetectorRequest' {} a -> s {state = a} :: UpdateDetectorRequest)

instance Prelude.Hashable UpdateDetectorRequest where
  hashWithSalt _salt UpdateDetectorRequest' {..} =
    _salt
      `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` detectorModelName
      `Prelude.hashWithSalt` state

instance Prelude.NFData UpdateDetectorRequest where
  rnf UpdateDetectorRequest' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf detectorModelName
      `Prelude.seq` Prelude.rnf state

instance Data.ToJSON UpdateDetectorRequest where
  toJSON UpdateDetectorRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyValue" Data..=) Prelude.<$> keyValue,
            Prelude.Just ("messageId" Data..= messageId),
            Prelude.Just
              ("detectorModelName" Data..= detectorModelName),
            Prelude.Just ("state" Data..= state)
          ]
      )

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
-- Module      : Amazonka.IoTEventsData.Types.DeleteDetectorRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DeleteDetectorRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information used to delete the detector model.
--
-- /See:/ 'newDeleteDetectorRequest' smart constructor.
data DeleteDetectorRequest = DeleteDetectorRequest'
  { -- | The value of the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateDetectorModel.html#iotevents-CreateDetectorModel-request-key key>
    -- used to identify the detector.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The ID to assign to the @DeleteDetectorRequest@. Each @\"messageId\"@
    -- must be unique within each batch sent.
    messageId :: Prelude.Text,
    -- | The name of the detector model that was used to create the detector
    -- instance.
    detectorModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDetectorRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'deleteDetectorRequest_keyValue' - The value of the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateDetectorModel.html#iotevents-CreateDetectorModel-request-key key>
-- used to identify the detector.
--
-- 'messageId', 'deleteDetectorRequest_messageId' - The ID to assign to the @DeleteDetectorRequest@. Each @\"messageId\"@
-- must be unique within each batch sent.
--
-- 'detectorModelName', 'deleteDetectorRequest_detectorModelName' - The name of the detector model that was used to create the detector
-- instance.
newDeleteDetectorRequest ::
  -- | 'messageId'
  Prelude.Text ->
  -- | 'detectorModelName'
  Prelude.Text ->
  DeleteDetectorRequest
newDeleteDetectorRequest
  pMessageId_
  pDetectorModelName_ =
    DeleteDetectorRequest'
      { keyValue = Prelude.Nothing,
        messageId = pMessageId_,
        detectorModelName = pDetectorModelName_
      }

-- | The value of the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateDetectorModel.html#iotevents-CreateDetectorModel-request-key key>
-- used to identify the detector.
deleteDetectorRequest_keyValue :: Lens.Lens' DeleteDetectorRequest (Prelude.Maybe Prelude.Text)
deleteDetectorRequest_keyValue = Lens.lens (\DeleteDetectorRequest' {keyValue} -> keyValue) (\s@DeleteDetectorRequest' {} a -> s {keyValue = a} :: DeleteDetectorRequest)

-- | The ID to assign to the @DeleteDetectorRequest@. Each @\"messageId\"@
-- must be unique within each batch sent.
deleteDetectorRequest_messageId :: Lens.Lens' DeleteDetectorRequest Prelude.Text
deleteDetectorRequest_messageId = Lens.lens (\DeleteDetectorRequest' {messageId} -> messageId) (\s@DeleteDetectorRequest' {} a -> s {messageId = a} :: DeleteDetectorRequest)

-- | The name of the detector model that was used to create the detector
-- instance.
deleteDetectorRequest_detectorModelName :: Lens.Lens' DeleteDetectorRequest Prelude.Text
deleteDetectorRequest_detectorModelName = Lens.lens (\DeleteDetectorRequest' {detectorModelName} -> detectorModelName) (\s@DeleteDetectorRequest' {} a -> s {detectorModelName = a} :: DeleteDetectorRequest)

instance Prelude.Hashable DeleteDetectorRequest where
  hashWithSalt _salt DeleteDetectorRequest' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData DeleteDetectorRequest where
  rnf DeleteDetectorRequest' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf detectorModelName

instance Core.ToJSON DeleteDetectorRequest where
  toJSON DeleteDetectorRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keyValue" Core..=) Prelude.<$> keyValue,
            Prelude.Just ("messageId" Core..= messageId),
            Prelude.Just
              ("detectorModelName" Core..= detectorModelName)
          ]
      )

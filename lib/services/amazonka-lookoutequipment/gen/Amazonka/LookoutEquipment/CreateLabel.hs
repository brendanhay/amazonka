{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutEquipment.CreateLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a label for an event.
module Amazonka.LookoutEquipment.CreateLabel
  ( -- * Creating a Request
    CreateLabel (..),
    newCreateLabel,

    -- * Request Lenses
    createLabel_equipment,
    createLabel_faultCode,
    createLabel_notes,
    createLabel_labelGroupName,
    createLabel_startTime,
    createLabel_endTime,
    createLabel_rating,
    createLabel_clientToken,

    -- * Destructuring the Response
    CreateLabelResponse (..),
    newCreateLabelResponse,

    -- * Response Lenses
    createLabelResponse_labelId,
    createLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLabel' smart constructor.
data CreateLabel = CreateLabel'
  { -- | Indicates that a label pertains to a particular piece of equipment.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    equipment :: Prelude.Maybe Prelude.Text,
    -- | Provides additional information about the label. The fault code must be
    -- defined in the FaultCodes attribute of the label group.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    faultCode :: Prelude.Maybe Prelude.Text,
    -- | Metadata providing additional information about the label.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    notes :: Prelude.Maybe Prelude.Text,
    -- | The name of a group of labels.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    labelGroupName :: Prelude.Text,
    -- | The start time of the labeled event.
    startTime :: Data.POSIX,
    -- | The end time of the labeled event.
    endTime :: Data.POSIX,
    -- | Indicates whether a labeled event represents an anomaly.
    rating :: LabelRating,
    -- | A unique identifier for the request to create a label. If you do not set
    -- the client request token, Lookout for Equipment generates one.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'equipment', 'createLabel_equipment' - Indicates that a label pertains to a particular piece of equipment.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'faultCode', 'createLabel_faultCode' - Provides additional information about the label. The fault code must be
-- defined in the FaultCodes attribute of the label group.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'notes', 'createLabel_notes' - Metadata providing additional information about the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'labelGroupName', 'createLabel_labelGroupName' - The name of a group of labels.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'startTime', 'createLabel_startTime' - The start time of the labeled event.
--
-- 'endTime', 'createLabel_endTime' - The end time of the labeled event.
--
-- 'rating', 'createLabel_rating' - Indicates whether a labeled event represents an anomaly.
--
-- 'clientToken', 'createLabel_clientToken' - A unique identifier for the request to create a label. If you do not set
-- the client request token, Lookout for Equipment generates one.
newCreateLabel ::
  -- | 'labelGroupName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'rating'
  LabelRating ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateLabel
newCreateLabel
  pLabelGroupName_
  pStartTime_
  pEndTime_
  pRating_
  pClientToken_ =
    CreateLabel'
      { equipment = Prelude.Nothing,
        faultCode = Prelude.Nothing,
        notes = Prelude.Nothing,
        labelGroupName = pLabelGroupName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        rating = pRating_,
        clientToken = pClientToken_
      }

-- | Indicates that a label pertains to a particular piece of equipment.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabel_equipment :: Lens.Lens' CreateLabel (Prelude.Maybe Prelude.Text)
createLabel_equipment = Lens.lens (\CreateLabel' {equipment} -> equipment) (\s@CreateLabel' {} a -> s {equipment = a} :: CreateLabel)

-- | Provides additional information about the label. The fault code must be
-- defined in the FaultCodes attribute of the label group.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabel_faultCode :: Lens.Lens' CreateLabel (Prelude.Maybe Prelude.Text)
createLabel_faultCode = Lens.lens (\CreateLabel' {faultCode} -> faultCode) (\s@CreateLabel' {} a -> s {faultCode = a} :: CreateLabel)

-- | Metadata providing additional information about the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabel_notes :: Lens.Lens' CreateLabel (Prelude.Maybe Prelude.Text)
createLabel_notes = Lens.lens (\CreateLabel' {notes} -> notes) (\s@CreateLabel' {} a -> s {notes = a} :: CreateLabel)

-- | The name of a group of labels.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabel_labelGroupName :: Lens.Lens' CreateLabel Prelude.Text
createLabel_labelGroupName = Lens.lens (\CreateLabel' {labelGroupName} -> labelGroupName) (\s@CreateLabel' {} a -> s {labelGroupName = a} :: CreateLabel)

-- | The start time of the labeled event.
createLabel_startTime :: Lens.Lens' CreateLabel Prelude.UTCTime
createLabel_startTime = Lens.lens (\CreateLabel' {startTime} -> startTime) (\s@CreateLabel' {} a -> s {startTime = a} :: CreateLabel) Prelude.. Data._Time

-- | The end time of the labeled event.
createLabel_endTime :: Lens.Lens' CreateLabel Prelude.UTCTime
createLabel_endTime = Lens.lens (\CreateLabel' {endTime} -> endTime) (\s@CreateLabel' {} a -> s {endTime = a} :: CreateLabel) Prelude.. Data._Time

-- | Indicates whether a labeled event represents an anomaly.
createLabel_rating :: Lens.Lens' CreateLabel LabelRating
createLabel_rating = Lens.lens (\CreateLabel' {rating} -> rating) (\s@CreateLabel' {} a -> s {rating = a} :: CreateLabel)

-- | A unique identifier for the request to create a label. If you do not set
-- the client request token, Lookout for Equipment generates one.
createLabel_clientToken :: Lens.Lens' CreateLabel Prelude.Text
createLabel_clientToken = Lens.lens (\CreateLabel' {clientToken} -> clientToken) (\s@CreateLabel' {} a -> s {clientToken = a} :: CreateLabel)

instance Core.AWSRequest CreateLabel where
  type AWSResponse CreateLabel = CreateLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLabelResponse'
            Prelude.<$> (x Data..?> "LabelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLabel where
  hashWithSalt _salt CreateLabel' {..} =
    _salt
      `Prelude.hashWithSalt` equipment
      `Prelude.hashWithSalt` faultCode
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` rating
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateLabel where
  rnf CreateLabel' {..} =
    Prelude.rnf equipment
      `Prelude.seq` Prelude.rnf faultCode
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf rating
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.CreateLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLabel where
  toJSON CreateLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Equipment" Data..=) Prelude.<$> equipment,
            ("FaultCode" Data..=) Prelude.<$> faultCode,
            ("Notes" Data..=) Prelude.<$> notes,
            Prelude.Just
              ("LabelGroupName" Data..= labelGroupName),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime),
            Prelude.Just ("Rating" Data..= rating),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLabelResponse' smart constructor.
data CreateLabelResponse = CreateLabelResponse'
  { -- | The ID of the label that you have created.
    labelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelId', 'createLabelResponse_labelId' - The ID of the label that you have created.
--
-- 'httpStatus', 'createLabelResponse_httpStatus' - The response's http status code.
newCreateLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLabelResponse
newCreateLabelResponse pHttpStatus_ =
  CreateLabelResponse'
    { labelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the label that you have created.
createLabelResponse_labelId :: Lens.Lens' CreateLabelResponse (Prelude.Maybe Prelude.Text)
createLabelResponse_labelId = Lens.lens (\CreateLabelResponse' {labelId} -> labelId) (\s@CreateLabelResponse' {} a -> s {labelId = a} :: CreateLabelResponse)

-- | The response's http status code.
createLabelResponse_httpStatus :: Lens.Lens' CreateLabelResponse Prelude.Int
createLabelResponse_httpStatus = Lens.lens (\CreateLabelResponse' {httpStatus} -> httpStatus) (\s@CreateLabelResponse' {} a -> s {httpStatus = a} :: CreateLabelResponse)

instance Prelude.NFData CreateLabelResponse where
  rnf CreateLabelResponse' {..} =
    Prelude.rnf labelId
      `Prelude.seq` Prelude.rnf httpStatus

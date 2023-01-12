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
-- Module      : Amazonka.FraudDetector.UpdateEventLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified event with a new label.
module Amazonka.FraudDetector.UpdateEventLabel
  ( -- * Creating a Request
    UpdateEventLabel (..),
    newUpdateEventLabel,

    -- * Request Lenses
    updateEventLabel_eventId,
    updateEventLabel_eventTypeName,
    updateEventLabel_assignedLabel,
    updateEventLabel_labelTimestamp,

    -- * Destructuring the Response
    UpdateEventLabelResponse (..),
    newUpdateEventLabelResponse,

    -- * Response Lenses
    updateEventLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventLabel' smart constructor.
data UpdateEventLabel = UpdateEventLabel'
  { -- | The ID of the event associated with the label to update.
    eventId :: Prelude.Text,
    -- | The event type of the event associated with the label to update.
    eventTypeName :: Prelude.Text,
    -- | The new label to assign to the event.
    assignedLabel :: Prelude.Text,
    -- | The timestamp associated with the label. The timestamp must be specified
    -- using ISO 8601 standard in UTC.
    labelTimestamp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'updateEventLabel_eventId' - The ID of the event associated with the label to update.
--
-- 'eventTypeName', 'updateEventLabel_eventTypeName' - The event type of the event associated with the label to update.
--
-- 'assignedLabel', 'updateEventLabel_assignedLabel' - The new label to assign to the event.
--
-- 'labelTimestamp', 'updateEventLabel_labelTimestamp' - The timestamp associated with the label. The timestamp must be specified
-- using ISO 8601 standard in UTC.
newUpdateEventLabel ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  -- | 'assignedLabel'
  Prelude.Text ->
  -- | 'labelTimestamp'
  Prelude.Text ->
  UpdateEventLabel
newUpdateEventLabel
  pEventId_
  pEventTypeName_
  pAssignedLabel_
  pLabelTimestamp_ =
    UpdateEventLabel'
      { eventId = pEventId_,
        eventTypeName = pEventTypeName_,
        assignedLabel = pAssignedLabel_,
        labelTimestamp = pLabelTimestamp_
      }

-- | The ID of the event associated with the label to update.
updateEventLabel_eventId :: Lens.Lens' UpdateEventLabel Prelude.Text
updateEventLabel_eventId = Lens.lens (\UpdateEventLabel' {eventId} -> eventId) (\s@UpdateEventLabel' {} a -> s {eventId = a} :: UpdateEventLabel)

-- | The event type of the event associated with the label to update.
updateEventLabel_eventTypeName :: Lens.Lens' UpdateEventLabel Prelude.Text
updateEventLabel_eventTypeName = Lens.lens (\UpdateEventLabel' {eventTypeName} -> eventTypeName) (\s@UpdateEventLabel' {} a -> s {eventTypeName = a} :: UpdateEventLabel)

-- | The new label to assign to the event.
updateEventLabel_assignedLabel :: Lens.Lens' UpdateEventLabel Prelude.Text
updateEventLabel_assignedLabel = Lens.lens (\UpdateEventLabel' {assignedLabel} -> assignedLabel) (\s@UpdateEventLabel' {} a -> s {assignedLabel = a} :: UpdateEventLabel)

-- | The timestamp associated with the label. The timestamp must be specified
-- using ISO 8601 standard in UTC.
updateEventLabel_labelTimestamp :: Lens.Lens' UpdateEventLabel Prelude.Text
updateEventLabel_labelTimestamp = Lens.lens (\UpdateEventLabel' {labelTimestamp} -> labelTimestamp) (\s@UpdateEventLabel' {} a -> s {labelTimestamp = a} :: UpdateEventLabel)

instance Core.AWSRequest UpdateEventLabel where
  type
    AWSResponse UpdateEventLabel =
      UpdateEventLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventLabelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventLabel where
  hashWithSalt _salt UpdateEventLabel' {..} =
    _salt `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` assignedLabel
      `Prelude.hashWithSalt` labelTimestamp

instance Prelude.NFData UpdateEventLabel where
  rnf UpdateEventLabel' {..} =
    Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf assignedLabel
      `Prelude.seq` Prelude.rnf labelTimestamp

instance Data.ToHeaders UpdateEventLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateEventLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEventLabel where
  toJSON UpdateEventLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventId" Data..= eventId),
            Prelude.Just ("eventTypeName" Data..= eventTypeName),
            Prelude.Just ("assignedLabel" Data..= assignedLabel),
            Prelude.Just
              ("labelTimestamp" Data..= labelTimestamp)
          ]
      )

instance Data.ToPath UpdateEventLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEventLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventLabelResponse' smart constructor.
data UpdateEventLabelResponse = UpdateEventLabelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEventLabelResponse_httpStatus' - The response's http status code.
newUpdateEventLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventLabelResponse
newUpdateEventLabelResponse pHttpStatus_ =
  UpdateEventLabelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEventLabelResponse_httpStatus :: Lens.Lens' UpdateEventLabelResponse Prelude.Int
updateEventLabelResponse_httpStatus = Lens.lens (\UpdateEventLabelResponse' {httpStatus} -> httpStatus) (\s@UpdateEventLabelResponse' {} a -> s {httpStatus = a} :: UpdateEventLabelResponse)

instance Prelude.NFData UpdateEventLabelResponse where
  rnf UpdateEventLabelResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.SSMContacts.UpdateRotation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information specified for an on-call rotation.
module Amazonka.SSMContacts.UpdateRotation
  ( -- * Creating a Request
    UpdateRotation (..),
    newUpdateRotation,

    -- * Request Lenses
    updateRotation_contactIds,
    updateRotation_startTime,
    updateRotation_timeZoneId,
    updateRotation_rotationId,
    updateRotation_recurrence,

    -- * Destructuring the Response
    UpdateRotationResponse (..),
    newUpdateRotationResponse,

    -- * Response Lenses
    updateRotationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newUpdateRotation' smart constructor.
data UpdateRotation = UpdateRotation'
  { -- | The Amazon Resource Names (ARNs) of the contacts to include in the
    -- updated rotation.
    --
    -- The order in which you list the contacts is their shift order in the
    -- rotation schedule.
    contactIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The date and time the rotation goes into effect.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The time zone to base the updated rotation’s activity on, in Internet
    -- Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
    -- information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    --
    -- Designators for time zones that don’t support Daylight Savings Time
    -- Rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
    -- (PDT), aren\'t supported.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rotation to update.
    rotationId :: Prelude.Text,
    -- | Information about how long the updated rotation lasts before restarting
    -- at the beginning of the shift order.
    recurrence :: RecurrenceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactIds', 'updateRotation_contactIds' - The Amazon Resource Names (ARNs) of the contacts to include in the
-- updated rotation.
--
-- The order in which you list the contacts is their shift order in the
-- rotation schedule.
--
-- 'startTime', 'updateRotation_startTime' - The date and time the rotation goes into effect.
--
-- 'timeZoneId', 'updateRotation_timeZoneId' - The time zone to base the updated rotation’s activity on, in Internet
-- Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- Designators for time zones that don’t support Daylight Savings Time
-- Rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
-- (PDT), aren\'t supported.
--
-- 'rotationId', 'updateRotation_rotationId' - The Amazon Resource Name (ARN) of the rotation to update.
--
-- 'recurrence', 'updateRotation_recurrence' - Information about how long the updated rotation lasts before restarting
-- at the beginning of the shift order.
newUpdateRotation ::
  -- | 'rotationId'
  Prelude.Text ->
  -- | 'recurrence'
  RecurrenceSettings ->
  UpdateRotation
newUpdateRotation pRotationId_ pRecurrence_ =
  UpdateRotation'
    { contactIds = Prelude.Nothing,
      startTime = Prelude.Nothing,
      timeZoneId = Prelude.Nothing,
      rotationId = pRotationId_,
      recurrence = pRecurrence_
    }

-- | The Amazon Resource Names (ARNs) of the contacts to include in the
-- updated rotation.
--
-- The order in which you list the contacts is their shift order in the
-- rotation schedule.
updateRotation_contactIds :: Lens.Lens' UpdateRotation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateRotation_contactIds = Lens.lens (\UpdateRotation' {contactIds} -> contactIds) (\s@UpdateRotation' {} a -> s {contactIds = a} :: UpdateRotation) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the rotation goes into effect.
updateRotation_startTime :: Lens.Lens' UpdateRotation (Prelude.Maybe Prelude.UTCTime)
updateRotation_startTime = Lens.lens (\UpdateRotation' {startTime} -> startTime) (\s@UpdateRotation' {} a -> s {startTime = a} :: UpdateRotation) Prelude.. Lens.mapping Data._Time

-- | The time zone to base the updated rotation’s activity on, in Internet
-- Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\". For more
-- information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- Designators for time zones that don’t support Daylight Savings Time
-- Rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
-- (PDT), aren\'t supported.
updateRotation_timeZoneId :: Lens.Lens' UpdateRotation (Prelude.Maybe Prelude.Text)
updateRotation_timeZoneId = Lens.lens (\UpdateRotation' {timeZoneId} -> timeZoneId) (\s@UpdateRotation' {} a -> s {timeZoneId = a} :: UpdateRotation)

-- | The Amazon Resource Name (ARN) of the rotation to update.
updateRotation_rotationId :: Lens.Lens' UpdateRotation Prelude.Text
updateRotation_rotationId = Lens.lens (\UpdateRotation' {rotationId} -> rotationId) (\s@UpdateRotation' {} a -> s {rotationId = a} :: UpdateRotation)

-- | Information about how long the updated rotation lasts before restarting
-- at the beginning of the shift order.
updateRotation_recurrence :: Lens.Lens' UpdateRotation RecurrenceSettings
updateRotation_recurrence = Lens.lens (\UpdateRotation' {recurrence} -> recurrence) (\s@UpdateRotation' {} a -> s {recurrence = a} :: UpdateRotation)

instance Core.AWSRequest UpdateRotation where
  type
    AWSResponse UpdateRotation =
      UpdateRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRotationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRotation where
  hashWithSalt _salt UpdateRotation' {..} =
    _salt
      `Prelude.hashWithSalt` contactIds
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` rotationId
      `Prelude.hashWithSalt` recurrence

instance Prelude.NFData UpdateRotation where
  rnf UpdateRotation' {..} =
    Prelude.rnf contactIds
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf rotationId
      `Prelude.seq` Prelude.rnf recurrence

instance Data.ToHeaders UpdateRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.UpdateRotation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRotation where
  toJSON UpdateRotation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContactIds" Data..=) Prelude.<$> contactIds,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("TimeZoneId" Data..=) Prelude.<$> timeZoneId,
            Prelude.Just ("RotationId" Data..= rotationId),
            Prelude.Just ("Recurrence" Data..= recurrence)
          ]
      )

instance Data.ToPath UpdateRotation where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRotationResponse' smart constructor.
data UpdateRotationResponse = UpdateRotationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRotationResponse_httpStatus' - The response's http status code.
newUpdateRotationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRotationResponse
newUpdateRotationResponse pHttpStatus_ =
  UpdateRotationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRotationResponse_httpStatus :: Lens.Lens' UpdateRotationResponse Prelude.Int
updateRotationResponse_httpStatus = Lens.lens (\UpdateRotationResponse' {httpStatus} -> httpStatus) (\s@UpdateRotationResponse' {} a -> s {httpStatus = a} :: UpdateRotationResponse)

instance Prelude.NFData UpdateRotationResponse where
  rnf UpdateRotationResponse' {..} =
    Prelude.rnf httpStatus

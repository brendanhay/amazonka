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
-- Module      : Amazonka.SSMContacts.GetRotation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an on-call rotation.
module Amazonka.SSMContacts.GetRotation
  ( -- * Creating a Request
    GetRotation (..),
    newGetRotation,

    -- * Request Lenses
    getRotation_rotationId,

    -- * Destructuring the Response
    GetRotationResponse (..),
    newGetRotationResponse,

    -- * Response Lenses
    getRotationResponse_httpStatus,
    getRotationResponse_rotationArn,
    getRotationResponse_name,
    getRotationResponse_contactIds,
    getRotationResponse_startTime,
    getRotationResponse_timeZoneId,
    getRotationResponse_recurrence,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newGetRotation' smart constructor.
data GetRotation = GetRotation'
  { -- | The Amazon Resource Name (ARN) of the on-call rotation to retrieve
    -- information about.
    rotationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationId', 'getRotation_rotationId' - The Amazon Resource Name (ARN) of the on-call rotation to retrieve
-- information about.
newGetRotation ::
  -- | 'rotationId'
  Prelude.Text ->
  GetRotation
newGetRotation pRotationId_ =
  GetRotation' {rotationId = pRotationId_}

-- | The Amazon Resource Name (ARN) of the on-call rotation to retrieve
-- information about.
getRotation_rotationId :: Lens.Lens' GetRotation Prelude.Text
getRotation_rotationId = Lens.lens (\GetRotation' {rotationId} -> rotationId) (\s@GetRotation' {} a -> s {rotationId = a} :: GetRotation)

instance Core.AWSRequest GetRotation where
  type AWSResponse GetRotation = GetRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRotationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RotationArn")
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "ContactIds")
            Prelude.<*> (x Data..:> "StartTime")
            Prelude.<*> (x Data..:> "TimeZoneId")
            Prelude.<*> (x Data..:> "Recurrence")
      )

instance Prelude.Hashable GetRotation where
  hashWithSalt _salt GetRotation' {..} =
    _salt `Prelude.hashWithSalt` rotationId

instance Prelude.NFData GetRotation where
  rnf GetRotation' {..} = Prelude.rnf rotationId

instance Data.ToHeaders GetRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.GetRotation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRotation where
  toJSON GetRotation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RotationId" Data..= rotationId)]
      )

instance Data.ToPath GetRotation where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRotationResponse' smart constructor.
data GetRotationResponse = GetRotationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the on-call rotation.
    rotationArn :: Prelude.Text,
    -- | The name of the on-call rotation.
    name :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the contacts assigned to the on-call
    -- rotation team.
    contactIds :: Prelude.NonEmpty Prelude.Text,
    -- | The specified start time for the on-call rotation.
    startTime :: Data.POSIX,
    -- | The time zone that the rotation’s activity is based on, in Internet
    -- Assigned Numbers Authority (IANA) format.
    timeZoneId :: Prelude.Text,
    -- | Specifies how long a rotation lasts before restarting at the beginning
    -- of the shift order.
    recurrence :: RecurrenceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRotationResponse_httpStatus' - The response's http status code.
--
-- 'rotationArn', 'getRotationResponse_rotationArn' - The Amazon Resource Name (ARN) of the on-call rotation.
--
-- 'name', 'getRotationResponse_name' - The name of the on-call rotation.
--
-- 'contactIds', 'getRotationResponse_contactIds' - The Amazon Resource Names (ARNs) of the contacts assigned to the on-call
-- rotation team.
--
-- 'startTime', 'getRotationResponse_startTime' - The specified start time for the on-call rotation.
--
-- 'timeZoneId', 'getRotationResponse_timeZoneId' - The time zone that the rotation’s activity is based on, in Internet
-- Assigned Numbers Authority (IANA) format.
--
-- 'recurrence', 'getRotationResponse_recurrence' - Specifies how long a rotation lasts before restarting at the beginning
-- of the shift order.
newGetRotationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'rotationArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'contactIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'timeZoneId'
  Prelude.Text ->
  -- | 'recurrence'
  RecurrenceSettings ->
  GetRotationResponse
newGetRotationResponse
  pHttpStatus_
  pRotationArn_
  pName_
  pContactIds_
  pStartTime_
  pTimeZoneId_
  pRecurrence_ =
    GetRotationResponse'
      { httpStatus = pHttpStatus_,
        rotationArn = pRotationArn_,
        name = pName_,
        contactIds = Lens.coerced Lens.# pContactIds_,
        startTime = Data._Time Lens.# pStartTime_,
        timeZoneId = pTimeZoneId_,
        recurrence = pRecurrence_
      }

-- | The response's http status code.
getRotationResponse_httpStatus :: Lens.Lens' GetRotationResponse Prelude.Int
getRotationResponse_httpStatus = Lens.lens (\GetRotationResponse' {httpStatus} -> httpStatus) (\s@GetRotationResponse' {} a -> s {httpStatus = a} :: GetRotationResponse)

-- | The Amazon Resource Name (ARN) of the on-call rotation.
getRotationResponse_rotationArn :: Lens.Lens' GetRotationResponse Prelude.Text
getRotationResponse_rotationArn = Lens.lens (\GetRotationResponse' {rotationArn} -> rotationArn) (\s@GetRotationResponse' {} a -> s {rotationArn = a} :: GetRotationResponse)

-- | The name of the on-call rotation.
getRotationResponse_name :: Lens.Lens' GetRotationResponse Prelude.Text
getRotationResponse_name = Lens.lens (\GetRotationResponse' {name} -> name) (\s@GetRotationResponse' {} a -> s {name = a} :: GetRotationResponse)

-- | The Amazon Resource Names (ARNs) of the contacts assigned to the on-call
-- rotation team.
getRotationResponse_contactIds :: Lens.Lens' GetRotationResponse (Prelude.NonEmpty Prelude.Text)
getRotationResponse_contactIds = Lens.lens (\GetRotationResponse' {contactIds} -> contactIds) (\s@GetRotationResponse' {} a -> s {contactIds = a} :: GetRotationResponse) Prelude.. Lens.coerced

-- | The specified start time for the on-call rotation.
getRotationResponse_startTime :: Lens.Lens' GetRotationResponse Prelude.UTCTime
getRotationResponse_startTime = Lens.lens (\GetRotationResponse' {startTime} -> startTime) (\s@GetRotationResponse' {} a -> s {startTime = a} :: GetRotationResponse) Prelude.. Data._Time

-- | The time zone that the rotation’s activity is based on, in Internet
-- Assigned Numbers Authority (IANA) format.
getRotationResponse_timeZoneId :: Lens.Lens' GetRotationResponse Prelude.Text
getRotationResponse_timeZoneId = Lens.lens (\GetRotationResponse' {timeZoneId} -> timeZoneId) (\s@GetRotationResponse' {} a -> s {timeZoneId = a} :: GetRotationResponse)

-- | Specifies how long a rotation lasts before restarting at the beginning
-- of the shift order.
getRotationResponse_recurrence :: Lens.Lens' GetRotationResponse RecurrenceSettings
getRotationResponse_recurrence = Lens.lens (\GetRotationResponse' {recurrence} -> recurrence) (\s@GetRotationResponse' {} a -> s {recurrence = a} :: GetRotationResponse)

instance Prelude.NFData GetRotationResponse where
  rnf GetRotationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rotationArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf contactIds
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf recurrence

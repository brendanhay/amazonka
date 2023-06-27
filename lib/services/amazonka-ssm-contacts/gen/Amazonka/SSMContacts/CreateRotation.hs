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
-- Module      : Amazonka.SSMContacts.CreateRotation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rotation in an on-call schedule.
module Amazonka.SSMContacts.CreateRotation
  ( -- * Creating a Request
    CreateRotation (..),
    newCreateRotation,

    -- * Request Lenses
    createRotation_idempotencyToken,
    createRotation_startTime,
    createRotation_tags,
    createRotation_name,
    createRotation_contactIds,
    createRotation_timeZoneId,
    createRotation_recurrence,

    -- * Destructuring the Response
    CreateRotationResponse (..),
    newCreateRotationResponse,

    -- * Response Lenses
    createRotationResponse_httpStatus,
    createRotationResponse_rotationArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newCreateRotation' smart constructor.
data CreateRotation = CreateRotation'
  { -- | A token that ensures that the operation is called only once with the
    -- specified details.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the rotation goes into effect.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Optional metadata to assign to the rotation. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/incident-manager/latest/userguide/tagging.html Tagging Incident Manager resources>
    -- in the /Incident Manager User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the rotation.
    name :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the contacts to add to the rotation.
    --
    -- The order that you list the contacts in is their shift order in the
    -- rotation schedule. To change the order of the contact\'s shifts, use the
    -- UpdateRotation operation.
    contactIds :: Prelude.NonEmpty Prelude.Text,
    -- | The time zone to base the rotation’s activity on in Internet Assigned
    -- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
    -- \"UTC\", or \"Asia\/Seoul\". For more information, see the
    -- <https://www.iana.org/time-zones Time Zone Database> on the IANA
    -- website.
    --
    -- Designators for time zones that don’t support Daylight Savings Time
    -- rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
    -- (PDT), are not supported.
    timeZoneId :: Prelude.Text,
    -- | Information about the rule that specifies when a shift\'s team members
    -- rotate.
    recurrence :: RecurrenceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createRotation_idempotencyToken' - A token that ensures that the operation is called only once with the
-- specified details.
--
-- 'startTime', 'createRotation_startTime' - The date and time that the rotation goes into effect.
--
-- 'tags', 'createRotation_tags' - Optional metadata to assign to the rotation. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/incident-manager/latest/userguide/tagging.html Tagging Incident Manager resources>
-- in the /Incident Manager User Guide/.
--
-- 'name', 'createRotation_name' - The name of the rotation.
--
-- 'contactIds', 'createRotation_contactIds' - The Amazon Resource Names (ARNs) of the contacts to add to the rotation.
--
-- The order that you list the contacts in is their shift order in the
-- rotation schedule. To change the order of the contact\'s shifts, use the
-- UpdateRotation operation.
--
-- 'timeZoneId', 'createRotation_timeZoneId' - The time zone to base the rotation’s activity on in Internet Assigned
-- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
-- \"UTC\", or \"Asia\/Seoul\". For more information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- Designators for time zones that don’t support Daylight Savings Time
-- rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
-- (PDT), are not supported.
--
-- 'recurrence', 'createRotation_recurrence' - Information about the rule that specifies when a shift\'s team members
-- rotate.
newCreateRotation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'contactIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'timeZoneId'
  Prelude.Text ->
  -- | 'recurrence'
  RecurrenceSettings ->
  CreateRotation
newCreateRotation
  pName_
  pContactIds_
  pTimeZoneId_
  pRecurrence_ =
    CreateRotation'
      { idempotencyToken = Prelude.Nothing,
        startTime = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        contactIds = Lens.coerced Lens.# pContactIds_,
        timeZoneId = pTimeZoneId_,
        recurrence = pRecurrence_
      }

-- | A token that ensures that the operation is called only once with the
-- specified details.
createRotation_idempotencyToken :: Lens.Lens' CreateRotation (Prelude.Maybe Prelude.Text)
createRotation_idempotencyToken = Lens.lens (\CreateRotation' {idempotencyToken} -> idempotencyToken) (\s@CreateRotation' {} a -> s {idempotencyToken = a} :: CreateRotation)

-- | The date and time that the rotation goes into effect.
createRotation_startTime :: Lens.Lens' CreateRotation (Prelude.Maybe Prelude.UTCTime)
createRotation_startTime = Lens.lens (\CreateRotation' {startTime} -> startTime) (\s@CreateRotation' {} a -> s {startTime = a} :: CreateRotation) Prelude.. Lens.mapping Data._Time

-- | Optional metadata to assign to the rotation. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/incident-manager/latest/userguide/tagging.html Tagging Incident Manager resources>
-- in the /Incident Manager User Guide/.
createRotation_tags :: Lens.Lens' CreateRotation (Prelude.Maybe [Tag])
createRotation_tags = Lens.lens (\CreateRotation' {tags} -> tags) (\s@CreateRotation' {} a -> s {tags = a} :: CreateRotation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rotation.
createRotation_name :: Lens.Lens' CreateRotation Prelude.Text
createRotation_name = Lens.lens (\CreateRotation' {name} -> name) (\s@CreateRotation' {} a -> s {name = a} :: CreateRotation)

-- | The Amazon Resource Names (ARNs) of the contacts to add to the rotation.
--
-- The order that you list the contacts in is their shift order in the
-- rotation schedule. To change the order of the contact\'s shifts, use the
-- UpdateRotation operation.
createRotation_contactIds :: Lens.Lens' CreateRotation (Prelude.NonEmpty Prelude.Text)
createRotation_contactIds = Lens.lens (\CreateRotation' {contactIds} -> contactIds) (\s@CreateRotation' {} a -> s {contactIds = a} :: CreateRotation) Prelude.. Lens.coerced

-- | The time zone to base the rotation’s activity on in Internet Assigned
-- Numbers Authority (IANA) format. For example: \"America\/Los_Angeles\",
-- \"UTC\", or \"Asia\/Seoul\". For more information, see the
-- <https://www.iana.org/time-zones Time Zone Database> on the IANA
-- website.
--
-- Designators for time zones that don’t support Daylight Savings Time
-- rules, such as Pacific Standard Time (PST) and Pacific Daylight Time
-- (PDT), are not supported.
createRotation_timeZoneId :: Lens.Lens' CreateRotation Prelude.Text
createRotation_timeZoneId = Lens.lens (\CreateRotation' {timeZoneId} -> timeZoneId) (\s@CreateRotation' {} a -> s {timeZoneId = a} :: CreateRotation)

-- | Information about the rule that specifies when a shift\'s team members
-- rotate.
createRotation_recurrence :: Lens.Lens' CreateRotation RecurrenceSettings
createRotation_recurrence = Lens.lens (\CreateRotation' {recurrence} -> recurrence) (\s@CreateRotation' {} a -> s {recurrence = a} :: CreateRotation)

instance Core.AWSRequest CreateRotation where
  type
    AWSResponse CreateRotation =
      CreateRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRotationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RotationArn")
      )

instance Prelude.Hashable CreateRotation where
  hashWithSalt _salt CreateRotation' {..} =
    _salt
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` contactIds
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` recurrence

instance Prelude.NFData CreateRotation where
  rnf CreateRotation' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf contactIds
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf recurrence

instance Data.ToHeaders CreateRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.CreateRotation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRotation where
  toJSON CreateRotation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ContactIds" Data..= contactIds),
            Prelude.Just ("TimeZoneId" Data..= timeZoneId),
            Prelude.Just ("Recurrence" Data..= recurrence)
          ]
      )

instance Data.ToPath CreateRotation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRotationResponse' smart constructor.
data CreateRotationResponse = CreateRotationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the created rotation.
    rotationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRotationResponse_httpStatus' - The response's http status code.
--
-- 'rotationArn', 'createRotationResponse_rotationArn' - The Amazon Resource Name (ARN) of the created rotation.
newCreateRotationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'rotationArn'
  Prelude.Text ->
  CreateRotationResponse
newCreateRotationResponse pHttpStatus_ pRotationArn_ =
  CreateRotationResponse'
    { httpStatus = pHttpStatus_,
      rotationArn = pRotationArn_
    }

-- | The response's http status code.
createRotationResponse_httpStatus :: Lens.Lens' CreateRotationResponse Prelude.Int
createRotationResponse_httpStatus = Lens.lens (\CreateRotationResponse' {httpStatus} -> httpStatus) (\s@CreateRotationResponse' {} a -> s {httpStatus = a} :: CreateRotationResponse)

-- | The Amazon Resource Name (ARN) of the created rotation.
createRotationResponse_rotationArn :: Lens.Lens' CreateRotationResponse Prelude.Text
createRotationResponse_rotationArn = Lens.lens (\CreateRotationResponse' {rotationArn} -> rotationArn) (\s@CreateRotationResponse' {} a -> s {rotationArn = a} :: CreateRotationResponse)

instance Prelude.NFData CreateRotationResponse where
  rnf CreateRotationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rotationArn

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
-- Module      : Amazonka.SSMContacts.CreateRotationOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an override for a rotation in an on-call schedule.
module Amazonka.SSMContacts.CreateRotationOverride
  ( -- * Creating a Request
    CreateRotationOverride (..),
    newCreateRotationOverride,

    -- * Request Lenses
    createRotationOverride_idempotencyToken,
    createRotationOverride_rotationId,
    createRotationOverride_newContactIds,
    createRotationOverride_startTime,
    createRotationOverride_endTime,

    -- * Destructuring the Response
    CreateRotationOverrideResponse (..),
    newCreateRotationOverrideResponse,

    -- * Response Lenses
    createRotationOverrideResponse_httpStatus,
    createRotationOverrideResponse_rotationOverrideId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newCreateRotationOverride' smart constructor.
data CreateRotationOverride = CreateRotationOverride'
  { -- | A token that ensures that the operation is called only once with the
    -- specified details.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rotation to create an override
    -- for.
    rotationId :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the contacts to replace those in the
    -- current on-call rotation with.
    --
    -- If you want to include any current team members in the override shift,
    -- you must include their ARNs in the new contact ID list.
    newContactIds' :: [Prelude.Text],
    -- | The date and time when the override goes into effect.
    startTime :: Data.POSIX,
    -- | The date and time when the override ends.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRotationOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createRotationOverride_idempotencyToken' - A token that ensures that the operation is called only once with the
-- specified details.
--
-- 'rotationId', 'createRotationOverride_rotationId' - The Amazon Resource Name (ARN) of the rotation to create an override
-- for.
--
-- 'newContactIds'', 'createRotationOverride_newContactIds' - The Amazon Resource Names (ARNs) of the contacts to replace those in the
-- current on-call rotation with.
--
-- If you want to include any current team members in the override shift,
-- you must include their ARNs in the new contact ID list.
--
-- 'startTime', 'createRotationOverride_startTime' - The date and time when the override goes into effect.
--
-- 'endTime', 'createRotationOverride_endTime' - The date and time when the override ends.
newCreateRotationOverride ::
  -- | 'rotationId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  CreateRotationOverride
newCreateRotationOverride
  pRotationId_
  pStartTime_
  pEndTime_ =
    CreateRotationOverride'
      { idempotencyToken =
          Prelude.Nothing,
        rotationId = pRotationId_,
        newContactIds' = Prelude.mempty,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | A token that ensures that the operation is called only once with the
-- specified details.
createRotationOverride_idempotencyToken :: Lens.Lens' CreateRotationOverride (Prelude.Maybe Prelude.Text)
createRotationOverride_idempotencyToken = Lens.lens (\CreateRotationOverride' {idempotencyToken} -> idempotencyToken) (\s@CreateRotationOverride' {} a -> s {idempotencyToken = a} :: CreateRotationOverride)

-- | The Amazon Resource Name (ARN) of the rotation to create an override
-- for.
createRotationOverride_rotationId :: Lens.Lens' CreateRotationOverride Prelude.Text
createRotationOverride_rotationId = Lens.lens (\CreateRotationOverride' {rotationId} -> rotationId) (\s@CreateRotationOverride' {} a -> s {rotationId = a} :: CreateRotationOverride)

-- | The Amazon Resource Names (ARNs) of the contacts to replace those in the
-- current on-call rotation with.
--
-- If you want to include any current team members in the override shift,
-- you must include their ARNs in the new contact ID list.
createRotationOverride_newContactIds :: Lens.Lens' CreateRotationOverride [Prelude.Text]
createRotationOverride_newContactIds = Lens.lens (\CreateRotationOverride' {newContactIds'} -> newContactIds') (\s@CreateRotationOverride' {} a -> s {newContactIds' = a} :: CreateRotationOverride) Prelude.. Lens.coerced

-- | The date and time when the override goes into effect.
createRotationOverride_startTime :: Lens.Lens' CreateRotationOverride Prelude.UTCTime
createRotationOverride_startTime = Lens.lens (\CreateRotationOverride' {startTime} -> startTime) (\s@CreateRotationOverride' {} a -> s {startTime = a} :: CreateRotationOverride) Prelude.. Data._Time

-- | The date and time when the override ends.
createRotationOverride_endTime :: Lens.Lens' CreateRotationOverride Prelude.UTCTime
createRotationOverride_endTime = Lens.lens (\CreateRotationOverride' {endTime} -> endTime) (\s@CreateRotationOverride' {} a -> s {endTime = a} :: CreateRotationOverride) Prelude.. Data._Time

instance Core.AWSRequest CreateRotationOverride where
  type
    AWSResponse CreateRotationOverride =
      CreateRotationOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRotationOverrideResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RotationOverrideId")
      )

instance Prelude.Hashable CreateRotationOverride where
  hashWithSalt _salt CreateRotationOverride' {..} =
    _salt
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` rotationId
      `Prelude.hashWithSalt` newContactIds'
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData CreateRotationOverride where
  rnf CreateRotationOverride' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf rotationId
      `Prelude.seq` Prelude.rnf newContactIds'
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders CreateRotationOverride where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.CreateRotationOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRotationOverride where
  toJSON CreateRotationOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            Prelude.Just ("RotationId" Data..= rotationId),
            Prelude.Just
              ("NewContactIds" Data..= newContactIds'),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath CreateRotationOverride where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRotationOverride where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRotationOverrideResponse' smart constructor.
data CreateRotationOverrideResponse = CreateRotationOverrideResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the created rotation override.
    rotationOverrideId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRotationOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRotationOverrideResponse_httpStatus' - The response's http status code.
--
-- 'rotationOverrideId', 'createRotationOverrideResponse_rotationOverrideId' - The Amazon Resource Name (ARN) of the created rotation override.
newCreateRotationOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'rotationOverrideId'
  Prelude.Text ->
  CreateRotationOverrideResponse
newCreateRotationOverrideResponse
  pHttpStatus_
  pRotationOverrideId_ =
    CreateRotationOverrideResponse'
      { httpStatus =
          pHttpStatus_,
        rotationOverrideId = pRotationOverrideId_
      }

-- | The response's http status code.
createRotationOverrideResponse_httpStatus :: Lens.Lens' CreateRotationOverrideResponse Prelude.Int
createRotationOverrideResponse_httpStatus = Lens.lens (\CreateRotationOverrideResponse' {httpStatus} -> httpStatus) (\s@CreateRotationOverrideResponse' {} a -> s {httpStatus = a} :: CreateRotationOverrideResponse)

-- | The Amazon Resource Name (ARN) of the created rotation override.
createRotationOverrideResponse_rotationOverrideId :: Lens.Lens' CreateRotationOverrideResponse Prelude.Text
createRotationOverrideResponse_rotationOverrideId = Lens.lens (\CreateRotationOverrideResponse' {rotationOverrideId} -> rotationOverrideId) (\s@CreateRotationOverrideResponse' {} a -> s {rotationOverrideId = a} :: CreateRotationOverrideResponse)

instance
  Prelude.NFData
    CreateRotationOverrideResponse
  where
  rnf CreateRotationOverrideResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rotationOverrideId

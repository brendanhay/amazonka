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
-- Module      : Amazonka.SSMContacts.GetRotationOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an override to an on-call rotation.
module Amazonka.SSMContacts.GetRotationOverride
  ( -- * Creating a Request
    GetRotationOverride (..),
    newGetRotationOverride,

    -- * Request Lenses
    getRotationOverride_rotationId,
    getRotationOverride_rotationOverrideId,

    -- * Destructuring the Response
    GetRotationOverrideResponse (..),
    newGetRotationOverrideResponse,

    -- * Response Lenses
    getRotationOverrideResponse_createTime,
    getRotationOverrideResponse_endTime,
    getRotationOverrideResponse_newContactIds,
    getRotationOverrideResponse_rotationArn,
    getRotationOverrideResponse_rotationOverrideId,
    getRotationOverrideResponse_startTime,
    getRotationOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newGetRotationOverride' smart constructor.
data GetRotationOverride = GetRotationOverride'
  { -- | The Amazon Resource Name (ARN) of the overridden rotation to retrieve
    -- information about.
    rotationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the on-call rotation override to
    -- retrieve information about.
    rotationOverrideId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRotationOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationId', 'getRotationOverride_rotationId' - The Amazon Resource Name (ARN) of the overridden rotation to retrieve
-- information about.
--
-- 'rotationOverrideId', 'getRotationOverride_rotationOverrideId' - The Amazon Resource Name (ARN) of the on-call rotation override to
-- retrieve information about.
newGetRotationOverride ::
  -- | 'rotationId'
  Prelude.Text ->
  -- | 'rotationOverrideId'
  Prelude.Text ->
  GetRotationOverride
newGetRotationOverride
  pRotationId_
  pRotationOverrideId_ =
    GetRotationOverride'
      { rotationId = pRotationId_,
        rotationOverrideId = pRotationOverrideId_
      }

-- | The Amazon Resource Name (ARN) of the overridden rotation to retrieve
-- information about.
getRotationOverride_rotationId :: Lens.Lens' GetRotationOverride Prelude.Text
getRotationOverride_rotationId = Lens.lens (\GetRotationOverride' {rotationId} -> rotationId) (\s@GetRotationOverride' {} a -> s {rotationId = a} :: GetRotationOverride)

-- | The Amazon Resource Name (ARN) of the on-call rotation override to
-- retrieve information about.
getRotationOverride_rotationOverrideId :: Lens.Lens' GetRotationOverride Prelude.Text
getRotationOverride_rotationOverrideId = Lens.lens (\GetRotationOverride' {rotationOverrideId} -> rotationOverrideId) (\s@GetRotationOverride' {} a -> s {rotationOverrideId = a} :: GetRotationOverride)

instance Core.AWSRequest GetRotationOverride where
  type
    AWSResponse GetRotationOverride =
      GetRotationOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRotationOverrideResponse'
            Prelude.<$> (x Data..?> "CreateTime")
            Prelude.<*> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "NewContactIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "RotationArn")
            Prelude.<*> (x Data..?> "RotationOverrideId")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRotationOverride where
  hashWithSalt _salt GetRotationOverride' {..} =
    _salt
      `Prelude.hashWithSalt` rotationId
      `Prelude.hashWithSalt` rotationOverrideId

instance Prelude.NFData GetRotationOverride where
  rnf GetRotationOverride' {..} =
    Prelude.rnf rotationId
      `Prelude.seq` Prelude.rnf rotationOverrideId

instance Data.ToHeaders GetRotationOverride where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.GetRotationOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRotationOverride where
  toJSON GetRotationOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RotationId" Data..= rotationId),
            Prelude.Just
              ("RotationOverrideId" Data..= rotationOverrideId)
          ]
      )

instance Data.ToPath GetRotationOverride where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRotationOverride where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRotationOverrideResponse' smart constructor.
data GetRotationOverrideResponse = GetRotationOverrideResponse'
  { -- | The date and time when the override was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the override ends.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Names (ARNs) of the contacts assigned to the
    -- override of the on-call rotation.
    newContactIds' :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the on-call rotation that was
    -- overridden.
    rotationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the override to an on-call rotation.
    rotationOverrideId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the override goes into effect.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRotationOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'getRotationOverrideResponse_createTime' - The date and time when the override was created.
--
-- 'endTime', 'getRotationOverrideResponse_endTime' - The date and time when the override ends.
--
-- 'newContactIds'', 'getRotationOverrideResponse_newContactIds' - The Amazon Resource Names (ARNs) of the contacts assigned to the
-- override of the on-call rotation.
--
-- 'rotationArn', 'getRotationOverrideResponse_rotationArn' - The Amazon Resource Name (ARN) of the on-call rotation that was
-- overridden.
--
-- 'rotationOverrideId', 'getRotationOverrideResponse_rotationOverrideId' - The Amazon Resource Name (ARN) of the override to an on-call rotation.
--
-- 'startTime', 'getRotationOverrideResponse_startTime' - The date and time when the override goes into effect.
--
-- 'httpStatus', 'getRotationOverrideResponse_httpStatus' - The response's http status code.
newGetRotationOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRotationOverrideResponse
newGetRotationOverrideResponse pHttpStatus_ =
  GetRotationOverrideResponse'
    { createTime =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      newContactIds' = Prelude.Nothing,
      rotationArn = Prelude.Nothing,
      rotationOverrideId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the override was created.
getRotationOverrideResponse_createTime :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe Prelude.UTCTime)
getRotationOverrideResponse_createTime = Lens.lens (\GetRotationOverrideResponse' {createTime} -> createTime) (\s@GetRotationOverrideResponse' {} a -> s {createTime = a} :: GetRotationOverrideResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time when the override ends.
getRotationOverrideResponse_endTime :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe Prelude.UTCTime)
getRotationOverrideResponse_endTime = Lens.lens (\GetRotationOverrideResponse' {endTime} -> endTime) (\s@GetRotationOverrideResponse' {} a -> s {endTime = a} :: GetRotationOverrideResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Names (ARNs) of the contacts assigned to the
-- override of the on-call rotation.
getRotationOverrideResponse_newContactIds :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe [Prelude.Text])
getRotationOverrideResponse_newContactIds = Lens.lens (\GetRotationOverrideResponse' {newContactIds'} -> newContactIds') (\s@GetRotationOverrideResponse' {} a -> s {newContactIds' = a} :: GetRotationOverrideResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the on-call rotation that was
-- overridden.
getRotationOverrideResponse_rotationArn :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe Prelude.Text)
getRotationOverrideResponse_rotationArn = Lens.lens (\GetRotationOverrideResponse' {rotationArn} -> rotationArn) (\s@GetRotationOverrideResponse' {} a -> s {rotationArn = a} :: GetRotationOverrideResponse)

-- | The Amazon Resource Name (ARN) of the override to an on-call rotation.
getRotationOverrideResponse_rotationOverrideId :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe Prelude.Text)
getRotationOverrideResponse_rotationOverrideId = Lens.lens (\GetRotationOverrideResponse' {rotationOverrideId} -> rotationOverrideId) (\s@GetRotationOverrideResponse' {} a -> s {rotationOverrideId = a} :: GetRotationOverrideResponse)

-- | The date and time when the override goes into effect.
getRotationOverrideResponse_startTime :: Lens.Lens' GetRotationOverrideResponse (Prelude.Maybe Prelude.UTCTime)
getRotationOverrideResponse_startTime = Lens.lens (\GetRotationOverrideResponse' {startTime} -> startTime) (\s@GetRotationOverrideResponse' {} a -> s {startTime = a} :: GetRotationOverrideResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getRotationOverrideResponse_httpStatus :: Lens.Lens' GetRotationOverrideResponse Prelude.Int
getRotationOverrideResponse_httpStatus = Lens.lens (\GetRotationOverrideResponse' {httpStatus} -> httpStatus) (\s@GetRotationOverrideResponse' {} a -> s {httpStatus = a} :: GetRotationOverrideResponse)

instance Prelude.NFData GetRotationOverrideResponse where
  rnf GetRotationOverrideResponse' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf newContactIds'
      `Prelude.seq` Prelude.rnf rotationArn
      `Prelude.seq` Prelude.rnf rotationOverrideId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus

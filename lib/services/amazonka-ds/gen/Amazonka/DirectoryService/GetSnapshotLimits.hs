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
-- Module      : Amazonka.DirectoryService.GetSnapshotLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the manual snapshot limits for a directory.
module Amazonka.DirectoryService.GetSnapshotLimits
  ( -- * Creating a Request
    GetSnapshotLimits (..),
    newGetSnapshotLimits,

    -- * Request Lenses
    getSnapshotLimits_directoryId,

    -- * Destructuring the Response
    GetSnapshotLimitsResponse (..),
    newGetSnapshotLimitsResponse,

    -- * Response Lenses
    getSnapshotLimitsResponse_snapshotLimits,
    getSnapshotLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the GetSnapshotLimits operation.
--
-- /See:/ 'newGetSnapshotLimits' smart constructor.
data GetSnapshotLimits = GetSnapshotLimits'
  { -- | Contains the identifier of the directory to obtain the limits for.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'getSnapshotLimits_directoryId' - Contains the identifier of the directory to obtain the limits for.
newGetSnapshotLimits ::
  -- | 'directoryId'
  Prelude.Text ->
  GetSnapshotLimits
newGetSnapshotLimits pDirectoryId_ =
  GetSnapshotLimits' {directoryId = pDirectoryId_}

-- | Contains the identifier of the directory to obtain the limits for.
getSnapshotLimits_directoryId :: Lens.Lens' GetSnapshotLimits Prelude.Text
getSnapshotLimits_directoryId = Lens.lens (\GetSnapshotLimits' {directoryId} -> directoryId) (\s@GetSnapshotLimits' {} a -> s {directoryId = a} :: GetSnapshotLimits)

instance Core.AWSRequest GetSnapshotLimits where
  type
    AWSResponse GetSnapshotLimits =
      GetSnapshotLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnapshotLimitsResponse'
            Prelude.<$> (x Core..?> "SnapshotLimits")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnapshotLimits where
  hashWithSalt _salt GetSnapshotLimits' {..} =
    _salt `Prelude.hashWithSalt` directoryId

instance Prelude.NFData GetSnapshotLimits where
  rnf GetSnapshotLimits' {..} = Prelude.rnf directoryId

instance Core.ToHeaders GetSnapshotLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.GetSnapshotLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSnapshotLimits where
  toJSON GetSnapshotLimits' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath GetSnapshotLimits where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSnapshotLimits where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the GetSnapshotLimits operation.
--
-- /See:/ 'newGetSnapshotLimitsResponse' smart constructor.
data GetSnapshotLimitsResponse = GetSnapshotLimitsResponse'
  { -- | A SnapshotLimits object that contains the manual snapshot limits for the
    -- specified directory.
    snapshotLimits :: Prelude.Maybe SnapshotLimits,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotLimits', 'getSnapshotLimitsResponse_snapshotLimits' - A SnapshotLimits object that contains the manual snapshot limits for the
-- specified directory.
--
-- 'httpStatus', 'getSnapshotLimitsResponse_httpStatus' - The response's http status code.
newGetSnapshotLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnapshotLimitsResponse
newGetSnapshotLimitsResponse pHttpStatus_ =
  GetSnapshotLimitsResponse'
    { snapshotLimits =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A SnapshotLimits object that contains the manual snapshot limits for the
-- specified directory.
getSnapshotLimitsResponse_snapshotLimits :: Lens.Lens' GetSnapshotLimitsResponse (Prelude.Maybe SnapshotLimits)
getSnapshotLimitsResponse_snapshotLimits = Lens.lens (\GetSnapshotLimitsResponse' {snapshotLimits} -> snapshotLimits) (\s@GetSnapshotLimitsResponse' {} a -> s {snapshotLimits = a} :: GetSnapshotLimitsResponse)

-- | The response's http status code.
getSnapshotLimitsResponse_httpStatus :: Lens.Lens' GetSnapshotLimitsResponse Prelude.Int
getSnapshotLimitsResponse_httpStatus = Lens.lens (\GetSnapshotLimitsResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotLimitsResponse' {} a -> s {httpStatus = a} :: GetSnapshotLimitsResponse)

instance Prelude.NFData GetSnapshotLimitsResponse where
  rnf GetSnapshotLimitsResponse' {..} =
    Prelude.rnf snapshotLimits
      `Prelude.seq` Prelude.rnf httpStatus

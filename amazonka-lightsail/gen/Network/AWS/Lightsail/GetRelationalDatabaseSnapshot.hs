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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database snapshot in Amazon
-- Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
  ( -- * Creating a Request
    GetRelationalDatabaseSnapshot (..),
    newGetRelationalDatabaseSnapshot,

    -- * Request Lenses
    getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,

    -- * Destructuring the Response
    GetRelationalDatabaseSnapshotResponse (..),
    newGetRelationalDatabaseSnapshotResponse,

    -- * Response Lenses
    getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot,
    getRelationalDatabaseSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseSnapshot' smart constructor.
data GetRelationalDatabaseSnapshot = GetRelationalDatabaseSnapshot'
  { -- | The name of the database snapshot for which to get information.
    relationalDatabaseSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseSnapshotName', 'getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName' - The name of the database snapshot for which to get information.
newGetRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseSnapshotName'
  Prelude.Text ->
  GetRelationalDatabaseSnapshot
newGetRelationalDatabaseSnapshot
  pRelationalDatabaseSnapshotName_ =
    GetRelationalDatabaseSnapshot'
      { relationalDatabaseSnapshotName =
          pRelationalDatabaseSnapshotName_
      }

-- | The name of the database snapshot for which to get information.
getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' GetRelationalDatabaseSnapshot Prelude.Text
getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName = Lens.lens (\GetRelationalDatabaseSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@GetRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: GetRelationalDatabaseSnapshot)

instance
  Core.AWSRequest
    GetRelationalDatabaseSnapshot
  where
  type
    AWSResponse GetRelationalDatabaseSnapshot =
      GetRelationalDatabaseSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseSnapshotResponse'
            Prelude.<$> (x Core..?> "relationalDatabaseSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseSnapshot

instance Prelude.NFData GetRelationalDatabaseSnapshot

instance Core.ToHeaders GetRelationalDatabaseSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabaseSnapshot where
  toJSON GetRelationalDatabaseSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseSnapshotName"
                  Core..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Core.ToPath GetRelationalDatabaseSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabaseSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseSnapshotResponse' smart constructor.
data GetRelationalDatabaseSnapshotResponse = GetRelationalDatabaseSnapshotResponse'
  { -- | An object describing the specified database snapshot.
    relationalDatabaseSnapshot :: Prelude.Maybe RelationalDatabaseSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseSnapshot', 'getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot' - An object describing the specified database snapshot.
--
-- 'httpStatus', 'getRelationalDatabaseSnapshotResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseSnapshotResponse
newGetRelationalDatabaseSnapshotResponse pHttpStatus_ =
  GetRelationalDatabaseSnapshotResponse'
    { relationalDatabaseSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object describing the specified database snapshot.
getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot :: Lens.Lens' GetRelationalDatabaseSnapshotResponse (Prelude.Maybe RelationalDatabaseSnapshot)
getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot = Lens.lens (\GetRelationalDatabaseSnapshotResponse' {relationalDatabaseSnapshot} -> relationalDatabaseSnapshot) (\s@GetRelationalDatabaseSnapshotResponse' {} a -> s {relationalDatabaseSnapshot = a} :: GetRelationalDatabaseSnapshotResponse)

-- | The response's http status code.
getRelationalDatabaseSnapshotResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseSnapshotResponse Prelude.Int
getRelationalDatabaseSnapshotResponse_httpStatus = Lens.lens (\GetRelationalDatabaseSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseSnapshotResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseSnapshotResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseSnapshotResponse

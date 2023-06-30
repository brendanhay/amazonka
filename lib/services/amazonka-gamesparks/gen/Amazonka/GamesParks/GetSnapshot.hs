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
-- Module      : Amazonka.GamesParks.GetSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a copy of the game configuration in a snapshot.
module Amazonka.GamesParks.GetSnapshot
  ( -- * Creating a Request
    GetSnapshot (..),
    newGetSnapshot,

    -- * Request Lenses
    getSnapshot_sections,
    getSnapshot_gameName,
    getSnapshot_snapshotId,

    -- * Destructuring the Response
    GetSnapshotResponse (..),
    newGetSnapshotResponse,

    -- * Response Lenses
    getSnapshotResponse_snapshot,
    getSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSnapshot' smart constructor.
data GetSnapshot = GetSnapshot'
  { -- | The list of game configuration sections to be described.
    sections :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sections', 'getSnapshot_sections' - The list of game configuration sections to be described.
--
-- 'gameName', 'getSnapshot_gameName' - The name of the game.
--
-- 'snapshotId', 'getSnapshot_snapshotId' - The identifier of the snapshot.
newGetSnapshot ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  GetSnapshot
newGetSnapshot pGameName_ pSnapshotId_ =
  GetSnapshot'
    { sections = Prelude.Nothing,
      gameName = pGameName_,
      snapshotId = pSnapshotId_
    }

-- | The list of game configuration sections to be described.
getSnapshot_sections :: Lens.Lens' GetSnapshot (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getSnapshot_sections = Lens.lens (\GetSnapshot' {sections} -> sections) (\s@GetSnapshot' {} a -> s {sections = a} :: GetSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the game.
getSnapshot_gameName :: Lens.Lens' GetSnapshot Prelude.Text
getSnapshot_gameName = Lens.lens (\GetSnapshot' {gameName} -> gameName) (\s@GetSnapshot' {} a -> s {gameName = a} :: GetSnapshot)

-- | The identifier of the snapshot.
getSnapshot_snapshotId :: Lens.Lens' GetSnapshot Prelude.Text
getSnapshot_snapshotId = Lens.lens (\GetSnapshot' {snapshotId} -> snapshotId) (\s@GetSnapshot' {} a -> s {snapshotId = a} :: GetSnapshot)

instance Core.AWSRequest GetSnapshot where
  type AWSResponse GetSnapshot = GetSnapshotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnapshotResponse'
            Prelude.<$> (x Data..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnapshot where
  hashWithSalt _salt GetSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` sections
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData GetSnapshot where
  rnf GetSnapshot' {..} =
    Prelude.rnf sections
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders GetSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSnapshot where
  toPath GetSnapshot' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/snapshot/",
        Data.toBS snapshotId
      ]

instance Data.ToQuery GetSnapshot where
  toQuery GetSnapshot' {..} =
    Prelude.mconcat
      [ "Sections"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> sections)
      ]

-- | /See:/ 'newGetSnapshotResponse' smart constructor.
data GetSnapshotResponse = GetSnapshotResponse'
  { -- | Properties that provide details of the snapshot.
    snapshot :: Prelude.Maybe SnapshotDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'getSnapshotResponse_snapshot' - Properties that provide details of the snapshot.
--
-- 'httpStatus', 'getSnapshotResponse_httpStatus' - The response's http status code.
newGetSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnapshotResponse
newGetSnapshotResponse pHttpStatus_ =
  GetSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that provide details of the snapshot.
getSnapshotResponse_snapshot :: Lens.Lens' GetSnapshotResponse (Prelude.Maybe SnapshotDetails)
getSnapshotResponse_snapshot = Lens.lens (\GetSnapshotResponse' {snapshot} -> snapshot) (\s@GetSnapshotResponse' {} a -> s {snapshot = a} :: GetSnapshotResponse)

-- | The response's http status code.
getSnapshotResponse_httpStatus :: Lens.Lens' GetSnapshotResponse Prelude.Int
getSnapshotResponse_httpStatus = Lens.lens (\GetSnapshotResponse' {httpStatus} -> httpStatus) (\s@GetSnapshotResponse' {} a -> s {httpStatus = a} :: GetSnapshotResponse)

instance Prelude.NFData GetSnapshotResponse where
  rnf GetSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus

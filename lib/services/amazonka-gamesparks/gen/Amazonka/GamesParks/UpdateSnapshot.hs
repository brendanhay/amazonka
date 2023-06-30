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
-- Module      : Amazonka.GamesParks.UpdateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the metadata of a GameSparks snapshot.
module Amazonka.GamesParks.UpdateSnapshot
  ( -- * Creating a Request
    UpdateSnapshot (..),
    newUpdateSnapshot,

    -- * Request Lenses
    updateSnapshot_description,
    updateSnapshot_gameName,
    updateSnapshot_snapshotId,

    -- * Destructuring the Response
    UpdateSnapshotResponse (..),
    newUpdateSnapshotResponse,

    -- * Response Lenses
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSnapshot' smart constructor.
data UpdateSnapshot = UpdateSnapshot'
  { -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateSnapshot_description' - The description of the snapshot.
--
-- 'gameName', 'updateSnapshot_gameName' - The name of the game.
--
-- 'snapshotId', 'updateSnapshot_snapshotId' - The identifier of the snapshot.
newUpdateSnapshot ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  UpdateSnapshot
newUpdateSnapshot pGameName_ pSnapshotId_ =
  UpdateSnapshot'
    { description = Prelude.Nothing,
      gameName = pGameName_,
      snapshotId = pSnapshotId_
    }

-- | The description of the snapshot.
updateSnapshot_description :: Lens.Lens' UpdateSnapshot (Prelude.Maybe Prelude.Text)
updateSnapshot_description = Lens.lens (\UpdateSnapshot' {description} -> description) (\s@UpdateSnapshot' {} a -> s {description = a} :: UpdateSnapshot)

-- | The name of the game.
updateSnapshot_gameName :: Lens.Lens' UpdateSnapshot Prelude.Text
updateSnapshot_gameName = Lens.lens (\UpdateSnapshot' {gameName} -> gameName) (\s@UpdateSnapshot' {} a -> s {gameName = a} :: UpdateSnapshot)

-- | The identifier of the snapshot.
updateSnapshot_snapshotId :: Lens.Lens' UpdateSnapshot Prelude.Text
updateSnapshot_snapshotId = Lens.lens (\UpdateSnapshot' {snapshotId} -> snapshotId) (\s@UpdateSnapshot' {} a -> s {snapshotId = a} :: UpdateSnapshot)

instance Core.AWSRequest UpdateSnapshot where
  type
    AWSResponse UpdateSnapshot =
      UpdateSnapshotResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSnapshotResponse'
            Prelude.<$> (x Data..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSnapshot where
  hashWithSalt _salt UpdateSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData UpdateSnapshot where
  rnf UpdateSnapshot' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders UpdateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSnapshot where
  toJSON UpdateSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateSnapshot where
  toPath UpdateSnapshot' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/snapshot/",
        Data.toBS snapshotId
      ]

instance Data.ToQuery UpdateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSnapshotResponse' smart constructor.
data UpdateSnapshotResponse = UpdateSnapshotResponse'
  { -- | Properties that provide details of the updated snapshot.
    snapshot :: Prelude.Maybe SnapshotDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'updateSnapshotResponse_snapshot' - Properties that provide details of the updated snapshot.
--
-- 'httpStatus', 'updateSnapshotResponse_httpStatus' - The response's http status code.
newUpdateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSnapshotResponse
newUpdateSnapshotResponse pHttpStatus_ =
  UpdateSnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that provide details of the updated snapshot.
updateSnapshotResponse_snapshot :: Lens.Lens' UpdateSnapshotResponse (Prelude.Maybe SnapshotDetails)
updateSnapshotResponse_snapshot = Lens.lens (\UpdateSnapshotResponse' {snapshot} -> snapshot) (\s@UpdateSnapshotResponse' {} a -> s {snapshot = a} :: UpdateSnapshotResponse)

-- | The response's http status code.
updateSnapshotResponse_httpStatus :: Lens.Lens' UpdateSnapshotResponse Prelude.Int
updateSnapshotResponse_httpStatus = Lens.lens (\UpdateSnapshotResponse' {httpStatus} -> httpStatus) (\s@UpdateSnapshotResponse' {} a -> s {httpStatus = a} :: UpdateSnapshotResponse)

instance Prelude.NFData UpdateSnapshotResponse where
  rnf UpdateSnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus

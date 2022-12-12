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
-- Module      : Amazonka.MGN.DeleteWave
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete wave.
module Amazonka.MGN.DeleteWave
  ( -- * Creating a Request
    DeleteWave (..),
    newDeleteWave,

    -- * Request Lenses
    deleteWave_waveID,

    -- * Destructuring the Response
    DeleteWaveResponse (..),
    newDeleteWaveResponse,

    -- * Response Lenses
    deleteWaveResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWave' smart constructor.
data DeleteWave = DeleteWave'
  { -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waveID', 'deleteWave_waveID' - Wave ID.
newDeleteWave ::
  -- | 'waveID'
  Prelude.Text ->
  DeleteWave
newDeleteWave pWaveID_ =
  DeleteWave' {waveID = pWaveID_}

-- | Wave ID.
deleteWave_waveID :: Lens.Lens' DeleteWave Prelude.Text
deleteWave_waveID = Lens.lens (\DeleteWave' {waveID} -> waveID) (\s@DeleteWave' {} a -> s {waveID = a} :: DeleteWave)

instance Core.AWSRequest DeleteWave where
  type AWSResponse DeleteWave = DeleteWaveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWaveResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWave where
  hashWithSalt _salt DeleteWave' {..} =
    _salt `Prelude.hashWithSalt` waveID

instance Prelude.NFData DeleteWave where
  rnf DeleteWave' {..} = Prelude.rnf waveID

instance Data.ToHeaders DeleteWave where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWave where
  toJSON DeleteWave' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("waveID" Data..= waveID)]
      )

instance Data.ToPath DeleteWave where
  toPath = Prelude.const "/DeleteWave"

instance Data.ToQuery DeleteWave where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWaveResponse' smart constructor.
data DeleteWaveResponse = DeleteWaveResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWaveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWaveResponse_httpStatus' - The response's http status code.
newDeleteWaveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWaveResponse
newDeleteWaveResponse pHttpStatus_ =
  DeleteWaveResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWaveResponse_httpStatus :: Lens.Lens' DeleteWaveResponse Prelude.Int
deleteWaveResponse_httpStatus = Lens.lens (\DeleteWaveResponse' {httpStatus} -> httpStatus) (\s@DeleteWaveResponse' {} a -> s {httpStatus = a} :: DeleteWaveResponse)

instance Prelude.NFData DeleteWaveResponse where
  rnf DeleteWaveResponse' {..} = Prelude.rnf httpStatus

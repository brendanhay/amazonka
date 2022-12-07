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
-- Module      : Amazonka.MediaLive.UpdateMultiplexProgram
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a program in a multiplex.
module Amazonka.MediaLive.UpdateMultiplexProgram
  ( -- * Creating a Request
    UpdateMultiplexProgram' (..),
    newUpdateMultiplexProgram',

    -- * Request Lenses
    updateMultiplexProgram'_multiplexProgramSettings,
    updateMultiplexProgram'_multiplexId,
    updateMultiplexProgram'_programName,

    -- * Destructuring the Response
    UpdateMultiplexProgramResponse (..),
    newUpdateMultiplexProgramResponse,

    -- * Response Lenses
    updateMultiplexProgramResponse_multiplexProgram,
    updateMultiplexProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update a program in a multiplex.
--
-- /See:/ 'newUpdateMultiplexProgram'' smart constructor.
data UpdateMultiplexProgram' = UpdateMultiplexProgram''
  { -- | The new settings for a multiplex program.
    multiplexProgramSettings :: Prelude.Maybe MultiplexProgramSettings,
    -- | The ID of the multiplex of the program to update.
    multiplexId :: Prelude.Text,
    -- | The name of the program to update.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMultiplexProgram'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexProgramSettings', 'updateMultiplexProgram'_multiplexProgramSettings' - The new settings for a multiplex program.
--
-- 'multiplexId', 'updateMultiplexProgram'_multiplexId' - The ID of the multiplex of the program to update.
--
-- 'programName', 'updateMultiplexProgram'_programName' - The name of the program to update.
newUpdateMultiplexProgram' ::
  -- | 'multiplexId'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  UpdateMultiplexProgram'
newUpdateMultiplexProgram'
  pMultiplexId_
  pProgramName_ =
    UpdateMultiplexProgram''
      { multiplexProgramSettings =
          Prelude.Nothing,
        multiplexId = pMultiplexId_,
        programName = pProgramName_
      }

-- | The new settings for a multiplex program.
updateMultiplexProgram'_multiplexProgramSettings :: Lens.Lens' UpdateMultiplexProgram' (Prelude.Maybe MultiplexProgramSettings)
updateMultiplexProgram'_multiplexProgramSettings = Lens.lens (\UpdateMultiplexProgram'' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@UpdateMultiplexProgram'' {} a -> s {multiplexProgramSettings = a} :: UpdateMultiplexProgram')

-- | The ID of the multiplex of the program to update.
updateMultiplexProgram'_multiplexId :: Lens.Lens' UpdateMultiplexProgram' Prelude.Text
updateMultiplexProgram'_multiplexId = Lens.lens (\UpdateMultiplexProgram'' {multiplexId} -> multiplexId) (\s@UpdateMultiplexProgram'' {} a -> s {multiplexId = a} :: UpdateMultiplexProgram')

-- | The name of the program to update.
updateMultiplexProgram'_programName :: Lens.Lens' UpdateMultiplexProgram' Prelude.Text
updateMultiplexProgram'_programName = Lens.lens (\UpdateMultiplexProgram'' {programName} -> programName) (\s@UpdateMultiplexProgram'' {} a -> s {programName = a} :: UpdateMultiplexProgram')

instance Core.AWSRequest UpdateMultiplexProgram' where
  type
    AWSResponse UpdateMultiplexProgram' =
      UpdateMultiplexProgramResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMultiplexProgramResponse'
            Prelude.<$> (x Data..?> "multiplexProgram")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMultiplexProgram' where
  hashWithSalt _salt UpdateMultiplexProgram'' {..} =
    _salt
      `Prelude.hashWithSalt` multiplexProgramSettings
      `Prelude.hashWithSalt` multiplexId
      `Prelude.hashWithSalt` programName

instance Prelude.NFData UpdateMultiplexProgram' where
  rnf UpdateMultiplexProgram'' {..} =
    Prelude.rnf multiplexProgramSettings
      `Prelude.seq` Prelude.rnf multiplexId
      `Prelude.seq` Prelude.rnf programName

instance Data.ToHeaders UpdateMultiplexProgram' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMultiplexProgram' where
  toJSON UpdateMultiplexProgram'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("multiplexProgramSettings" Data..=)
              Prelude.<$> multiplexProgramSettings
          ]
      )

instance Data.ToPath UpdateMultiplexProgram' where
  toPath UpdateMultiplexProgram'' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Data.toBS multiplexId,
        "/programs/",
        Data.toBS programName
      ]

instance Data.ToQuery UpdateMultiplexProgram' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateMultiplexProgramResponse
--
-- /See:/ 'newUpdateMultiplexProgramResponse' smart constructor.
data UpdateMultiplexProgramResponse = UpdateMultiplexProgramResponse'
  { -- | The updated multiplex program.
    multiplexProgram :: Prelude.Maybe MultiplexProgram,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMultiplexProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexProgram', 'updateMultiplexProgramResponse_multiplexProgram' - The updated multiplex program.
--
-- 'httpStatus', 'updateMultiplexProgramResponse_httpStatus' - The response's http status code.
newUpdateMultiplexProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMultiplexProgramResponse
newUpdateMultiplexProgramResponse pHttpStatus_ =
  UpdateMultiplexProgramResponse'
    { multiplexProgram =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated multiplex program.
updateMultiplexProgramResponse_multiplexProgram :: Lens.Lens' UpdateMultiplexProgramResponse (Prelude.Maybe MultiplexProgram)
updateMultiplexProgramResponse_multiplexProgram = Lens.lens (\UpdateMultiplexProgramResponse' {multiplexProgram} -> multiplexProgram) (\s@UpdateMultiplexProgramResponse' {} a -> s {multiplexProgram = a} :: UpdateMultiplexProgramResponse)

-- | The response's http status code.
updateMultiplexProgramResponse_httpStatus :: Lens.Lens' UpdateMultiplexProgramResponse Prelude.Int
updateMultiplexProgramResponse_httpStatus = Lens.lens (\UpdateMultiplexProgramResponse' {httpStatus} -> httpStatus) (\s@UpdateMultiplexProgramResponse' {} a -> s {httpStatus = a} :: UpdateMultiplexProgramResponse)

instance
  Prelude.NFData
    UpdateMultiplexProgramResponse
  where
  rnf UpdateMultiplexProgramResponse' {..} =
    Prelude.rnf multiplexProgram
      `Prelude.seq` Prelude.rnf httpStatus

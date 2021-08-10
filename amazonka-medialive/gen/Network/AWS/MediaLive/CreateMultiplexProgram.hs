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
-- Module      : Network.AWS.MediaLive.CreateMultiplexProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new program in the multiplex.
module Network.AWS.MediaLive.CreateMultiplexProgram
  ( -- * Creating a Request
    CreateMultiplexProgram' (..),
    newCreateMultiplexProgram',

    -- * Request Lenses
    createMultiplexProgram'_multiplexId,
    createMultiplexProgram'_requestId,
    createMultiplexProgram'_multiplexProgramSettings,
    createMultiplexProgram'_programName,

    -- * Destructuring the Response
    CreateMultiplexProgramResponse (..),
    newCreateMultiplexProgramResponse,

    -- * Response Lenses
    createMultiplexProgramResponse_multiplexProgram,
    createMultiplexProgramResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a program in a multiplex.
--
-- /See:/ 'newCreateMultiplexProgram'' smart constructor.
data CreateMultiplexProgram' = CreateMultiplexProgram''
  { -- | ID of the multiplex where the program is to be created.
    multiplexId :: Prelude.Text,
    -- | Unique request ID. This prevents retries from creating multiple
    -- resources.
    requestId :: Prelude.Text,
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: MultiplexProgramSettings,
    -- | Name of multiplex program.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultiplexProgram'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'createMultiplexProgram'_multiplexId' - ID of the multiplex where the program is to be created.
--
-- 'requestId', 'createMultiplexProgram'_requestId' - Unique request ID. This prevents retries from creating multiple
-- resources.
--
-- 'multiplexProgramSettings', 'createMultiplexProgram'_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'programName', 'createMultiplexProgram'_programName' - Name of multiplex program.
newCreateMultiplexProgram' ::
  -- | 'multiplexId'
  Prelude.Text ->
  -- | 'requestId'
  Prelude.Text ->
  -- | 'multiplexProgramSettings'
  MultiplexProgramSettings ->
  -- | 'programName'
  Prelude.Text ->
  CreateMultiplexProgram'
newCreateMultiplexProgram'
  pMultiplexId_
  pRequestId_
  pMultiplexProgramSettings_
  pProgramName_ =
    CreateMultiplexProgram''
      { multiplexId =
          pMultiplexId_,
        requestId = pRequestId_,
        multiplexProgramSettings =
          pMultiplexProgramSettings_,
        programName = pProgramName_
      }

-- | ID of the multiplex where the program is to be created.
createMultiplexProgram'_multiplexId :: Lens.Lens' CreateMultiplexProgram' Prelude.Text
createMultiplexProgram'_multiplexId = Lens.lens (\CreateMultiplexProgram'' {multiplexId} -> multiplexId) (\s@CreateMultiplexProgram'' {} a -> s {multiplexId = a} :: CreateMultiplexProgram')

-- | Unique request ID. This prevents retries from creating multiple
-- resources.
createMultiplexProgram'_requestId :: Lens.Lens' CreateMultiplexProgram' Prelude.Text
createMultiplexProgram'_requestId = Lens.lens (\CreateMultiplexProgram'' {requestId} -> requestId) (\s@CreateMultiplexProgram'' {} a -> s {requestId = a} :: CreateMultiplexProgram')

-- | The settings for this multiplex program.
createMultiplexProgram'_multiplexProgramSettings :: Lens.Lens' CreateMultiplexProgram' MultiplexProgramSettings
createMultiplexProgram'_multiplexProgramSettings = Lens.lens (\CreateMultiplexProgram'' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@CreateMultiplexProgram'' {} a -> s {multiplexProgramSettings = a} :: CreateMultiplexProgram')

-- | Name of multiplex program.
createMultiplexProgram'_programName :: Lens.Lens' CreateMultiplexProgram' Prelude.Text
createMultiplexProgram'_programName = Lens.lens (\CreateMultiplexProgram'' {programName} -> programName) (\s@CreateMultiplexProgram'' {} a -> s {programName = a} :: CreateMultiplexProgram')

instance Core.AWSRequest CreateMultiplexProgram' where
  type
    AWSResponse CreateMultiplexProgram' =
      CreateMultiplexProgramResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultiplexProgramResponse'
            Prelude.<$> (x Core..?> "multiplexProgram")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMultiplexProgram'

instance Prelude.NFData CreateMultiplexProgram'

instance Core.ToHeaders CreateMultiplexProgram' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMultiplexProgram' where
  toJSON CreateMultiplexProgram'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("requestId" Core..= requestId),
            Prelude.Just
              ( "multiplexProgramSettings"
                  Core..= multiplexProgramSettings
              ),
            Prelude.Just ("programName" Core..= programName)
          ]
      )

instance Core.ToPath CreateMultiplexProgram' where
  toPath CreateMultiplexProgram'' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/programs"
      ]

instance Core.ToQuery CreateMultiplexProgram' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for CreateMultiplexProgramResponse
--
-- /See:/ 'newCreateMultiplexProgramResponse' smart constructor.
data CreateMultiplexProgramResponse = CreateMultiplexProgramResponse'
  { -- | The newly created multiplex program.
    multiplexProgram :: Prelude.Maybe MultiplexProgram,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultiplexProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexProgram', 'createMultiplexProgramResponse_multiplexProgram' - The newly created multiplex program.
--
-- 'httpStatus', 'createMultiplexProgramResponse_httpStatus' - The response's http status code.
newCreateMultiplexProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMultiplexProgramResponse
newCreateMultiplexProgramResponse pHttpStatus_ =
  CreateMultiplexProgramResponse'
    { multiplexProgram =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created multiplex program.
createMultiplexProgramResponse_multiplexProgram :: Lens.Lens' CreateMultiplexProgramResponse (Prelude.Maybe MultiplexProgram)
createMultiplexProgramResponse_multiplexProgram = Lens.lens (\CreateMultiplexProgramResponse' {multiplexProgram} -> multiplexProgram) (\s@CreateMultiplexProgramResponse' {} a -> s {multiplexProgram = a} :: CreateMultiplexProgramResponse)

-- | The response's http status code.
createMultiplexProgramResponse_httpStatus :: Lens.Lens' CreateMultiplexProgramResponse Prelude.Int
createMultiplexProgramResponse_httpStatus = Lens.lens (\CreateMultiplexProgramResponse' {httpStatus} -> httpStatus) (\s@CreateMultiplexProgramResponse' {} a -> s {httpStatus = a} :: CreateMultiplexProgramResponse)

instance
  Prelude.NFData
    CreateMultiplexProgramResponse

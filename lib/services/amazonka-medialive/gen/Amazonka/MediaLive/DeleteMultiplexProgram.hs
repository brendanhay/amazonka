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
-- Module      : Amazonka.MediaLive.DeleteMultiplexProgram
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a program from a multiplex.
module Amazonka.MediaLive.DeleteMultiplexProgram
  ( -- * Creating a Request
    DeleteMultiplexProgram (..),
    newDeleteMultiplexProgram,

    -- * Request Lenses
    deleteMultiplexProgram_multiplexId,
    deleteMultiplexProgram_programName,

    -- * Destructuring the Response
    DeleteMultiplexProgramResponse (..),
    newDeleteMultiplexProgramResponse,

    -- * Response Lenses
    deleteMultiplexProgramResponse_programName,
    deleteMultiplexProgramResponse_packetIdentifiersMap,
    deleteMultiplexProgramResponse_pipelineDetails,
    deleteMultiplexProgramResponse_multiplexProgramSettings,
    deleteMultiplexProgramResponse_channelId,
    deleteMultiplexProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DeleteMultiplexProgramRequest
--
-- /See:/ 'newDeleteMultiplexProgram' smart constructor.
data DeleteMultiplexProgram = DeleteMultiplexProgram'
  { -- | The ID of the multiplex that the program belongs to.
    multiplexId :: Prelude.Text,
    -- | The multiplex program name.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMultiplexProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'deleteMultiplexProgram_multiplexId' - The ID of the multiplex that the program belongs to.
--
-- 'programName', 'deleteMultiplexProgram_programName' - The multiplex program name.
newDeleteMultiplexProgram ::
  -- | 'multiplexId'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  DeleteMultiplexProgram
newDeleteMultiplexProgram pMultiplexId_ pProgramName_ =
  DeleteMultiplexProgram'
    { multiplexId =
        pMultiplexId_,
      programName = pProgramName_
    }

-- | The ID of the multiplex that the program belongs to.
deleteMultiplexProgram_multiplexId :: Lens.Lens' DeleteMultiplexProgram Prelude.Text
deleteMultiplexProgram_multiplexId = Lens.lens (\DeleteMultiplexProgram' {multiplexId} -> multiplexId) (\s@DeleteMultiplexProgram' {} a -> s {multiplexId = a} :: DeleteMultiplexProgram)

-- | The multiplex program name.
deleteMultiplexProgram_programName :: Lens.Lens' DeleteMultiplexProgram Prelude.Text
deleteMultiplexProgram_programName = Lens.lens (\DeleteMultiplexProgram' {programName} -> programName) (\s@DeleteMultiplexProgram' {} a -> s {programName = a} :: DeleteMultiplexProgram)

instance Core.AWSRequest DeleteMultiplexProgram where
  type
    AWSResponse DeleteMultiplexProgram =
      DeleteMultiplexProgramResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMultiplexProgramResponse'
            Prelude.<$> (x Core..?> "programName")
            Prelude.<*> (x Core..?> "packetIdentifiersMap")
            Prelude.<*> ( x Core..?> "pipelineDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "multiplexProgramSettings")
            Prelude.<*> (x Core..?> "channelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMultiplexProgram where
  hashWithSalt _salt DeleteMultiplexProgram' {..} =
    _salt `Prelude.hashWithSalt` multiplexId
      `Prelude.hashWithSalt` programName

instance Prelude.NFData DeleteMultiplexProgram where
  rnf DeleteMultiplexProgram' {..} =
    Prelude.rnf multiplexId
      `Prelude.seq` Prelude.rnf programName

instance Core.ToHeaders DeleteMultiplexProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteMultiplexProgram where
  toPath DeleteMultiplexProgram' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/programs/",
        Core.toBS programName
      ]

instance Core.ToQuery DeleteMultiplexProgram where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteMultiplexProgramResponse
--
-- /See:/ 'newDeleteMultiplexProgramResponse' smart constructor.
data DeleteMultiplexProgramResponse = DeleteMultiplexProgramResponse'
  { -- | The name of the multiplex program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The packet identifier map for this multiplex program.
    packetIdentifiersMap :: Prelude.Maybe MultiplexProgramPacketIdentifiersMap,
    -- | Contains information about the current sources for the specified program
    -- in the specified multiplex. Keep in mind that each multiplex pipeline
    -- connects to both pipelines in a given source channel (the channel
    -- identified by the program). But only one of those channel pipelines is
    -- ever active at one time.
    pipelineDetails :: Prelude.Maybe [MultiplexProgramPipelineDetail],
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Prelude.Maybe MultiplexProgramSettings,
    -- | The MediaLive channel associated with the program.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMultiplexProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'programName', 'deleteMultiplexProgramResponse_programName' - The name of the multiplex program.
--
-- 'packetIdentifiersMap', 'deleteMultiplexProgramResponse_packetIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- 'pipelineDetails', 'deleteMultiplexProgramResponse_pipelineDetails' - Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
--
-- 'multiplexProgramSettings', 'deleteMultiplexProgramResponse_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'channelId', 'deleteMultiplexProgramResponse_channelId' - The MediaLive channel associated with the program.
--
-- 'httpStatus', 'deleteMultiplexProgramResponse_httpStatus' - The response's http status code.
newDeleteMultiplexProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMultiplexProgramResponse
newDeleteMultiplexProgramResponse pHttpStatus_ =
  DeleteMultiplexProgramResponse'
    { programName =
        Prelude.Nothing,
      packetIdentifiersMap = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      multiplexProgramSettings = Prelude.Nothing,
      channelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the multiplex program.
deleteMultiplexProgramResponse_programName :: Lens.Lens' DeleteMultiplexProgramResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexProgramResponse_programName = Lens.lens (\DeleteMultiplexProgramResponse' {programName} -> programName) (\s@DeleteMultiplexProgramResponse' {} a -> s {programName = a} :: DeleteMultiplexProgramResponse)

-- | The packet identifier map for this multiplex program.
deleteMultiplexProgramResponse_packetIdentifiersMap :: Lens.Lens' DeleteMultiplexProgramResponse (Prelude.Maybe MultiplexProgramPacketIdentifiersMap)
deleteMultiplexProgramResponse_packetIdentifiersMap = Lens.lens (\DeleteMultiplexProgramResponse' {packetIdentifiersMap} -> packetIdentifiersMap) (\s@DeleteMultiplexProgramResponse' {} a -> s {packetIdentifiersMap = a} :: DeleteMultiplexProgramResponse)

-- | Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
deleteMultiplexProgramResponse_pipelineDetails :: Lens.Lens' DeleteMultiplexProgramResponse (Prelude.Maybe [MultiplexProgramPipelineDetail])
deleteMultiplexProgramResponse_pipelineDetails = Lens.lens (\DeleteMultiplexProgramResponse' {pipelineDetails} -> pipelineDetails) (\s@DeleteMultiplexProgramResponse' {} a -> s {pipelineDetails = a} :: DeleteMultiplexProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The settings for this multiplex program.
deleteMultiplexProgramResponse_multiplexProgramSettings :: Lens.Lens' DeleteMultiplexProgramResponse (Prelude.Maybe MultiplexProgramSettings)
deleteMultiplexProgramResponse_multiplexProgramSettings = Lens.lens (\DeleteMultiplexProgramResponse' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@DeleteMultiplexProgramResponse' {} a -> s {multiplexProgramSettings = a} :: DeleteMultiplexProgramResponse)

-- | The MediaLive channel associated with the program.
deleteMultiplexProgramResponse_channelId :: Lens.Lens' DeleteMultiplexProgramResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexProgramResponse_channelId = Lens.lens (\DeleteMultiplexProgramResponse' {channelId} -> channelId) (\s@DeleteMultiplexProgramResponse' {} a -> s {channelId = a} :: DeleteMultiplexProgramResponse)

-- | The response's http status code.
deleteMultiplexProgramResponse_httpStatus :: Lens.Lens' DeleteMultiplexProgramResponse Prelude.Int
deleteMultiplexProgramResponse_httpStatus = Lens.lens (\DeleteMultiplexProgramResponse' {httpStatus} -> httpStatus) (\s@DeleteMultiplexProgramResponse' {} a -> s {httpStatus = a} :: DeleteMultiplexProgramResponse)

instance
  Prelude.NFData
    DeleteMultiplexProgramResponse
  where
  rnf DeleteMultiplexProgramResponse' {..} =
    Prelude.rnf programName
      `Prelude.seq` Prelude.rnf packetIdentifiersMap
      `Prelude.seq` Prelude.rnf pipelineDetails
      `Prelude.seq` Prelude.rnf multiplexProgramSettings
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf httpStatus

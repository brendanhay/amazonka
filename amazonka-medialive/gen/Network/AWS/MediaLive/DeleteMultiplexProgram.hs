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
-- Module      : Network.AWS.MediaLive.DeleteMultiplexProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a program from a multiplex.
module Network.AWS.MediaLive.DeleteMultiplexProgram
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
    deleteMultiplexProgramResponse_packetIdentifiersMap,
    deleteMultiplexProgramResponse_multiplexProgramSettings,
    deleteMultiplexProgramResponse_channelId,
    deleteMultiplexProgramResponse_programName,
    deleteMultiplexProgramResponse_pipelineDetails,
    deleteMultiplexProgramResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexProgramRequest
--
-- /See:/ 'newDeleteMultiplexProgram' smart constructor.
data DeleteMultiplexProgram = DeleteMultiplexProgram'
  { -- | The ID of the multiplex that the program belongs to.
    multiplexId :: Core.Text,
    -- | The multiplex program name.
    programName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'programName'
  Core.Text ->
  DeleteMultiplexProgram
newDeleteMultiplexProgram pMultiplexId_ pProgramName_ =
  DeleteMultiplexProgram'
    { multiplexId =
        pMultiplexId_,
      programName = pProgramName_
    }

-- | The ID of the multiplex that the program belongs to.
deleteMultiplexProgram_multiplexId :: Lens.Lens' DeleteMultiplexProgram Core.Text
deleteMultiplexProgram_multiplexId = Lens.lens (\DeleteMultiplexProgram' {multiplexId} -> multiplexId) (\s@DeleteMultiplexProgram' {} a -> s {multiplexId = a} :: DeleteMultiplexProgram)

-- | The multiplex program name.
deleteMultiplexProgram_programName :: Lens.Lens' DeleteMultiplexProgram Core.Text
deleteMultiplexProgram_programName = Lens.lens (\DeleteMultiplexProgram' {programName} -> programName) (\s@DeleteMultiplexProgram' {} a -> s {programName = a} :: DeleteMultiplexProgram)

instance Core.AWSRequest DeleteMultiplexProgram where
  type
    AWSResponse DeleteMultiplexProgram =
      DeleteMultiplexProgramResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMultiplexProgramResponse'
            Core.<$> (x Core..?> "packetIdentifiersMap")
            Core.<*> (x Core..?> "multiplexProgramSettings")
            Core.<*> (x Core..?> "channelId")
            Core.<*> (x Core..?> "programName")
            Core.<*> (x Core..?> "pipelineDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMultiplexProgram

instance Core.NFData DeleteMultiplexProgram

instance Core.ToHeaders DeleteMultiplexProgram where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteMultiplexProgram where
  toPath DeleteMultiplexProgram' {..} =
    Core.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/programs/",
        Core.toBS programName
      ]

instance Core.ToQuery DeleteMultiplexProgram where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteMultiplexProgramResponse
--
-- /See:/ 'newDeleteMultiplexProgramResponse' smart constructor.
data DeleteMultiplexProgramResponse = DeleteMultiplexProgramResponse'
  { -- | The packet identifier map for this multiplex program.
    packetIdentifiersMap :: Core.Maybe MultiplexProgramPacketIdentifiersMap,
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Core.Maybe MultiplexProgramSettings,
    -- | The MediaLive channel associated with the program.
    channelId :: Core.Maybe Core.Text,
    -- | The name of the multiplex program.
    programName :: Core.Maybe Core.Text,
    -- | Contains information about the current sources for the specified program
    -- in the specified multiplex. Keep in mind that each multiplex pipeline
    -- connects to both pipelines in a given source channel (the channel
    -- identified by the program). But only one of those channel pipelines is
    -- ever active at one time.
    pipelineDetails :: Core.Maybe [MultiplexProgramPipelineDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMultiplexProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetIdentifiersMap', 'deleteMultiplexProgramResponse_packetIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- 'multiplexProgramSettings', 'deleteMultiplexProgramResponse_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'channelId', 'deleteMultiplexProgramResponse_channelId' - The MediaLive channel associated with the program.
--
-- 'programName', 'deleteMultiplexProgramResponse_programName' - The name of the multiplex program.
--
-- 'pipelineDetails', 'deleteMultiplexProgramResponse_pipelineDetails' - Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
--
-- 'httpStatus', 'deleteMultiplexProgramResponse_httpStatus' - The response's http status code.
newDeleteMultiplexProgramResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMultiplexProgramResponse
newDeleteMultiplexProgramResponse pHttpStatus_ =
  DeleteMultiplexProgramResponse'
    { packetIdentifiersMap =
        Core.Nothing,
      multiplexProgramSettings = Core.Nothing,
      channelId = Core.Nothing,
      programName = Core.Nothing,
      pipelineDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The packet identifier map for this multiplex program.
deleteMultiplexProgramResponse_packetIdentifiersMap :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe MultiplexProgramPacketIdentifiersMap)
deleteMultiplexProgramResponse_packetIdentifiersMap = Lens.lens (\DeleteMultiplexProgramResponse' {packetIdentifiersMap} -> packetIdentifiersMap) (\s@DeleteMultiplexProgramResponse' {} a -> s {packetIdentifiersMap = a} :: DeleteMultiplexProgramResponse)

-- | The settings for this multiplex program.
deleteMultiplexProgramResponse_multiplexProgramSettings :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe MultiplexProgramSettings)
deleteMultiplexProgramResponse_multiplexProgramSettings = Lens.lens (\DeleteMultiplexProgramResponse' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@DeleteMultiplexProgramResponse' {} a -> s {multiplexProgramSettings = a} :: DeleteMultiplexProgramResponse)

-- | The MediaLive channel associated with the program.
deleteMultiplexProgramResponse_channelId :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Core.Text)
deleteMultiplexProgramResponse_channelId = Lens.lens (\DeleteMultiplexProgramResponse' {channelId} -> channelId) (\s@DeleteMultiplexProgramResponse' {} a -> s {channelId = a} :: DeleteMultiplexProgramResponse)

-- | The name of the multiplex program.
deleteMultiplexProgramResponse_programName :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Core.Text)
deleteMultiplexProgramResponse_programName = Lens.lens (\DeleteMultiplexProgramResponse' {programName} -> programName) (\s@DeleteMultiplexProgramResponse' {} a -> s {programName = a} :: DeleteMultiplexProgramResponse)

-- | Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
deleteMultiplexProgramResponse_pipelineDetails :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe [MultiplexProgramPipelineDetail])
deleteMultiplexProgramResponse_pipelineDetails = Lens.lens (\DeleteMultiplexProgramResponse' {pipelineDetails} -> pipelineDetails) (\s@DeleteMultiplexProgramResponse' {} a -> s {pipelineDetails = a} :: DeleteMultiplexProgramResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteMultiplexProgramResponse_httpStatus :: Lens.Lens' DeleteMultiplexProgramResponse Core.Int
deleteMultiplexProgramResponse_httpStatus = Lens.lens (\DeleteMultiplexProgramResponse' {httpStatus} -> httpStatus) (\s@DeleteMultiplexProgramResponse' {} a -> s {httpStatus = a} :: DeleteMultiplexProgramResponse)

instance Core.NFData DeleteMultiplexProgramResponse

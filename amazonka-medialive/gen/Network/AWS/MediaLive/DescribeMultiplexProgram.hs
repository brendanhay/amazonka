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
-- Module      : Network.AWS.MediaLive.DescribeMultiplexProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a program in a multiplex.
module Network.AWS.MediaLive.DescribeMultiplexProgram
  ( -- * Creating a Request
    DescribeMultiplexProgram (..),
    newDescribeMultiplexProgram,

    -- * Request Lenses
    describeMultiplexProgram_multiplexId,
    describeMultiplexProgram_programName,

    -- * Destructuring the Response
    DescribeMultiplexProgramResponse (..),
    newDescribeMultiplexProgramResponse,

    -- * Response Lenses
    describeMultiplexProgramResponse_packetIdentifiersMap,
    describeMultiplexProgramResponse_multiplexProgramSettings,
    describeMultiplexProgramResponse_channelId,
    describeMultiplexProgramResponse_programName,
    describeMultiplexProgramResponse_pipelineDetails,
    describeMultiplexProgramResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'newDescribeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'
  { -- | The ID of the multiplex that the program belongs to.
    multiplexId :: Core.Text,
    -- | The name of the program.
    programName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMultiplexProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'describeMultiplexProgram_multiplexId' - The ID of the multiplex that the program belongs to.
--
-- 'programName', 'describeMultiplexProgram_programName' - The name of the program.
newDescribeMultiplexProgram ::
  -- | 'multiplexId'
  Core.Text ->
  -- | 'programName'
  Core.Text ->
  DescribeMultiplexProgram
newDescribeMultiplexProgram
  pMultiplexId_
  pProgramName_ =
    DescribeMultiplexProgram'
      { multiplexId =
          pMultiplexId_,
        programName = pProgramName_
      }

-- | The ID of the multiplex that the program belongs to.
describeMultiplexProgram_multiplexId :: Lens.Lens' DescribeMultiplexProgram Core.Text
describeMultiplexProgram_multiplexId = Lens.lens (\DescribeMultiplexProgram' {multiplexId} -> multiplexId) (\s@DescribeMultiplexProgram' {} a -> s {multiplexId = a} :: DescribeMultiplexProgram)

-- | The name of the program.
describeMultiplexProgram_programName :: Lens.Lens' DescribeMultiplexProgram Core.Text
describeMultiplexProgram_programName = Lens.lens (\DescribeMultiplexProgram' {programName} -> programName) (\s@DescribeMultiplexProgram' {} a -> s {programName = a} :: DescribeMultiplexProgram)

instance Core.AWSRequest DescribeMultiplexProgram where
  type
    AWSResponse DescribeMultiplexProgram =
      DescribeMultiplexProgramResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMultiplexProgramResponse'
            Core.<$> (x Core..?> "packetIdentifiersMap")
            Core.<*> (x Core..?> "multiplexProgramSettings")
            Core.<*> (x Core..?> "channelId")
            Core.<*> (x Core..?> "programName")
            Core.<*> (x Core..?> "pipelineDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMultiplexProgram

instance Core.NFData DescribeMultiplexProgram

instance Core.ToHeaders DescribeMultiplexProgram where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeMultiplexProgram where
  toPath DescribeMultiplexProgram' {..} =
    Core.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/programs/",
        Core.toBS programName
      ]

instance Core.ToQuery DescribeMultiplexProgram where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'newDescribeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'
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
-- Create a value of 'DescribeMultiplexProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetIdentifiersMap', 'describeMultiplexProgramResponse_packetIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- 'multiplexProgramSettings', 'describeMultiplexProgramResponse_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'channelId', 'describeMultiplexProgramResponse_channelId' - The MediaLive channel associated with the program.
--
-- 'programName', 'describeMultiplexProgramResponse_programName' - The name of the multiplex program.
--
-- 'pipelineDetails', 'describeMultiplexProgramResponse_pipelineDetails' - Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
--
-- 'httpStatus', 'describeMultiplexProgramResponse_httpStatus' - The response's http status code.
newDescribeMultiplexProgramResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMultiplexProgramResponse
newDescribeMultiplexProgramResponse pHttpStatus_ =
  DescribeMultiplexProgramResponse'
    { packetIdentifiersMap =
        Core.Nothing,
      multiplexProgramSettings = Core.Nothing,
      channelId = Core.Nothing,
      programName = Core.Nothing,
      pipelineDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The packet identifier map for this multiplex program.
describeMultiplexProgramResponse_packetIdentifiersMap :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe MultiplexProgramPacketIdentifiersMap)
describeMultiplexProgramResponse_packetIdentifiersMap = Lens.lens (\DescribeMultiplexProgramResponse' {packetIdentifiersMap} -> packetIdentifiersMap) (\s@DescribeMultiplexProgramResponse' {} a -> s {packetIdentifiersMap = a} :: DescribeMultiplexProgramResponse)

-- | The settings for this multiplex program.
describeMultiplexProgramResponse_multiplexProgramSettings :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe MultiplexProgramSettings)
describeMultiplexProgramResponse_multiplexProgramSettings = Lens.lens (\DescribeMultiplexProgramResponse' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@DescribeMultiplexProgramResponse' {} a -> s {multiplexProgramSettings = a} :: DescribeMultiplexProgramResponse)

-- | The MediaLive channel associated with the program.
describeMultiplexProgramResponse_channelId :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
describeMultiplexProgramResponse_channelId = Lens.lens (\DescribeMultiplexProgramResponse' {channelId} -> channelId) (\s@DescribeMultiplexProgramResponse' {} a -> s {channelId = a} :: DescribeMultiplexProgramResponse)

-- | The name of the multiplex program.
describeMultiplexProgramResponse_programName :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
describeMultiplexProgramResponse_programName = Lens.lens (\DescribeMultiplexProgramResponse' {programName} -> programName) (\s@DescribeMultiplexProgramResponse' {} a -> s {programName = a} :: DescribeMultiplexProgramResponse)

-- | Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
describeMultiplexProgramResponse_pipelineDetails :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe [MultiplexProgramPipelineDetail])
describeMultiplexProgramResponse_pipelineDetails = Lens.lens (\DescribeMultiplexProgramResponse' {pipelineDetails} -> pipelineDetails) (\s@DescribeMultiplexProgramResponse' {} a -> s {pipelineDetails = a} :: DescribeMultiplexProgramResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMultiplexProgramResponse_httpStatus :: Lens.Lens' DescribeMultiplexProgramResponse Core.Int
describeMultiplexProgramResponse_httpStatus = Lens.lens (\DescribeMultiplexProgramResponse' {httpStatus} -> httpStatus) (\s@DescribeMultiplexProgramResponse' {} a -> s {httpStatus = a} :: DescribeMultiplexProgramResponse)

instance Core.NFData DescribeMultiplexProgramResponse

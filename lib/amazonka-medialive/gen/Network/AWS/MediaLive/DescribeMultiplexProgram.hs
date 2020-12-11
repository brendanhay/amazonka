{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a program in a multiplex.
module Network.AWS.MediaLive.DescribeMultiplexProgram
  ( -- * Creating a request
    DescribeMultiplexProgram (..),
    mkDescribeMultiplexProgram,

    -- ** Request lenses
    desMultiplexId,
    desProgramName,

    -- * Destructuring the response
    DescribeMultiplexProgramResponse (..),
    mkDescribeMultiplexProgramResponse,

    -- ** Response lenses
    dmpmrsPacketIdentifiersMap,
    dmpmrsPipelineDetails,
    dmpmrsProgramName,
    dmpmrsChannelId,
    dmpmrsMultiplexProgramSettings,
    dmpmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'mkDescribeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'
  { multiplexId ::
      Lude.Text,
    programName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMultiplexProgram' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex that the program belongs to.
-- * 'programName' - The name of the program.
mkDescribeMultiplexProgram ::
  -- | 'multiplexId'
  Lude.Text ->
  -- | 'programName'
  Lude.Text ->
  DescribeMultiplexProgram
mkDescribeMultiplexProgram pMultiplexId_ pProgramName_ =
  DescribeMultiplexProgram'
    { multiplexId = pMultiplexId_,
      programName = pProgramName_
    }

-- | The ID of the multiplex that the program belongs to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMultiplexId :: Lens.Lens' DescribeMultiplexProgram Lude.Text
desMultiplexId = Lens.lens (multiplexId :: DescribeMultiplexProgram -> Lude.Text) (\s a -> s {multiplexId = a} :: DescribeMultiplexProgram)
{-# DEPRECATED desMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The name of the program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desProgramName :: Lens.Lens' DescribeMultiplexProgram Lude.Text
desProgramName = Lens.lens (programName :: DescribeMultiplexProgram -> Lude.Text) (\s a -> s {programName = a} :: DescribeMultiplexProgram)
{-# DEPRECATED desProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Lude.AWSRequest DescribeMultiplexProgram where
  type Rs DescribeMultiplexProgram = DescribeMultiplexProgramResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMultiplexProgramResponse'
            Lude.<$> (x Lude..?> "packetIdentifiersMap")
            Lude.<*> (x Lude..?> "pipelineDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "programName")
            Lude.<*> (x Lude..?> "channelId")
            Lude.<*> (x Lude..?> "multiplexProgramSettings")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMultiplexProgram where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeMultiplexProgram where
  toPath DescribeMultiplexProgram' {..} =
    Lude.mconcat
      [ "/prod/multiplexes/",
        Lude.toBS multiplexId,
        "/programs/",
        Lude.toBS programName
      ]

instance Lude.ToQuery DescribeMultiplexProgram where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'mkDescribeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'
  { packetIdentifiersMap ::
      Lude.Maybe
        MultiplexProgramPacketIdentifiersMap,
    pipelineDetails ::
      Lude.Maybe
        [MultiplexProgramPipelineDetail],
    programName ::
      Lude.Maybe Lude.Text,
    channelId ::
      Lude.Maybe Lude.Text,
    multiplexProgramSettings ::
      Lude.Maybe
        MultiplexProgramSettings,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- * 'channelId' - The MediaLive channel associated with the program.
-- * 'multiplexProgramSettings' - The settings for this multiplex program.
-- * 'packetIdentifiersMap' - The packet identifier map for this multiplex program.
-- * 'pipelineDetails' - Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
-- * 'programName' - The name of the multiplex program.
-- * 'responseStatus' - The response status code.
mkDescribeMultiplexProgramResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMultiplexProgramResponse
mkDescribeMultiplexProgramResponse pResponseStatus_ =
  DescribeMultiplexProgramResponse'
    { packetIdentifiersMap =
        Lude.Nothing,
      pipelineDetails = Lude.Nothing,
      programName = Lude.Nothing,
      channelId = Lude.Nothing,
      multiplexProgramSettings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsPacketIdentifiersMap :: Lens.Lens' DescribeMultiplexProgramResponse (Lude.Maybe MultiplexProgramPacketIdentifiersMap)
dmpmrsPacketIdentifiersMap = Lens.lens (packetIdentifiersMap :: DescribeMultiplexProgramResponse -> Lude.Maybe MultiplexProgramPacketIdentifiersMap) (\s a -> s {packetIdentifiersMap = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsPacketIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead." #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsPipelineDetails :: Lens.Lens' DescribeMultiplexProgramResponse (Lude.Maybe [MultiplexProgramPipelineDetail])
dmpmrsPipelineDetails = Lens.lens (pipelineDetails :: DescribeMultiplexProgramResponse -> Lude.Maybe [MultiplexProgramPipelineDetail]) (\s a -> s {pipelineDetails = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsProgramName :: Lens.Lens' DescribeMultiplexProgramResponse (Lude.Maybe Lude.Text)
dmpmrsProgramName = Lens.lens (programName :: DescribeMultiplexProgramResponse -> Lude.Maybe Lude.Text) (\s a -> s {programName = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsChannelId :: Lens.Lens' DescribeMultiplexProgramResponse (Lude.Maybe Lude.Text)
dmpmrsChannelId = Lens.lens (channelId :: DescribeMultiplexProgramResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsMultiplexProgramSettings :: Lens.Lens' DescribeMultiplexProgramResponse (Lude.Maybe MultiplexProgramSettings)
dmpmrsMultiplexProgramSettings = Lens.lens (multiplexProgramSettings :: DescribeMultiplexProgramResponse -> Lude.Maybe MultiplexProgramSettings) (\s a -> s {multiplexProgramSettings = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpmrsResponseStatus :: Lens.Lens' DescribeMultiplexProgramResponse Lude.Int
dmpmrsResponseStatus = Lens.lens (responseStatus :: DescribeMultiplexProgramResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMultiplexProgramResponse)
{-# DEPRECATED dmpmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a program from a multiplex.
module Network.AWS.MediaLive.DeleteMultiplexProgram
  ( -- * Creating a request
    DeleteMultiplexProgram (..),
    mkDeleteMultiplexProgram,

    -- ** Request lenses
    dmpMultiplexId,
    dmpProgramName,

    -- * Destructuring the response
    DeleteMultiplexProgramResponse (..),
    mkDeleteMultiplexProgramResponse,

    -- ** Response lenses
    dmprsPacketIdentifiersMap,
    dmprsPipelineDetails,
    dmprsProgramName,
    dmprsChannelId,
    dmprsMultiplexProgramSettings,
    dmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteMultiplexProgramRequest
--
-- /See:/ 'mkDeleteMultiplexProgram' smart constructor.
data DeleteMultiplexProgram = DeleteMultiplexProgram'
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

-- | Creates a value of 'DeleteMultiplexProgram' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex that the program belongs to.
-- * 'programName' - The multiplex program name.
mkDeleteMultiplexProgram ::
  -- | 'multiplexId'
  Lude.Text ->
  -- | 'programName'
  Lude.Text ->
  DeleteMultiplexProgram
mkDeleteMultiplexProgram pMultiplexId_ pProgramName_ =
  DeleteMultiplexProgram'
    { multiplexId = pMultiplexId_,
      programName = pProgramName_
    }

-- | The ID of the multiplex that the program belongs to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpMultiplexId :: Lens.Lens' DeleteMultiplexProgram Lude.Text
dmpMultiplexId = Lens.lens (multiplexId :: DeleteMultiplexProgram -> Lude.Text) (\s a -> s {multiplexId = a} :: DeleteMultiplexProgram)
{-# DEPRECATED dmpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The multiplex program name.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpProgramName :: Lens.Lens' DeleteMultiplexProgram Lude.Text
dmpProgramName = Lens.lens (programName :: DeleteMultiplexProgram -> Lude.Text) (\s a -> s {programName = a} :: DeleteMultiplexProgram)
{-# DEPRECATED dmpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Lude.AWSRequest DeleteMultiplexProgram where
  type Rs DeleteMultiplexProgram = DeleteMultiplexProgramResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMultiplexProgramResponse'
            Lude.<$> (x Lude..?> "packetIdentifiersMap")
            Lude.<*> (x Lude..?> "pipelineDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "programName")
            Lude.<*> (x Lude..?> "channelId")
            Lude.<*> (x Lude..?> "multiplexProgramSettings")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMultiplexProgram where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteMultiplexProgram where
  toPath DeleteMultiplexProgram' {..} =
    Lude.mconcat
      [ "/prod/multiplexes/",
        Lude.toBS multiplexId,
        "/programs/",
        Lude.toBS programName
      ]

instance Lude.ToQuery DeleteMultiplexProgram where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteMultiplexProgramResponse
--
-- /See:/ 'mkDeleteMultiplexProgramResponse' smart constructor.
data DeleteMultiplexProgramResponse = DeleteMultiplexProgramResponse'
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
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- * 'channelId' - The MediaLive channel associated with the program.
-- * 'multiplexProgramSettings' - The settings for this multiplex program.
-- * 'packetIdentifiersMap' - The packet identifier map for this multiplex program.
-- * 'pipelineDetails' - Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
-- * 'programName' - The name of the multiplex program.
-- * 'responseStatus' - The response status code.
mkDeleteMultiplexProgramResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMultiplexProgramResponse
mkDeleteMultiplexProgramResponse pResponseStatus_ =
  DeleteMultiplexProgramResponse'
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
dmprsPacketIdentifiersMap :: Lens.Lens' DeleteMultiplexProgramResponse (Lude.Maybe MultiplexProgramPacketIdentifiersMap)
dmprsPacketIdentifiersMap = Lens.lens (packetIdentifiersMap :: DeleteMultiplexProgramResponse -> Lude.Maybe MultiplexProgramPacketIdentifiersMap) (\s a -> s {packetIdentifiersMap = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsPacketIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead." #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsPipelineDetails :: Lens.Lens' DeleteMultiplexProgramResponse (Lude.Maybe [MultiplexProgramPipelineDetail])
dmprsPipelineDetails = Lens.lens (pipelineDetails :: DeleteMultiplexProgramResponse -> Lude.Maybe [MultiplexProgramPipelineDetail]) (\s a -> s {pipelineDetails = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsProgramName :: Lens.Lens' DeleteMultiplexProgramResponse (Lude.Maybe Lude.Text)
dmprsProgramName = Lens.lens (programName :: DeleteMultiplexProgramResponse -> Lude.Maybe Lude.Text) (\s a -> s {programName = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsChannelId :: Lens.Lens' DeleteMultiplexProgramResponse (Lude.Maybe Lude.Text)
dmprsChannelId = Lens.lens (channelId :: DeleteMultiplexProgramResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsMultiplexProgramSettings :: Lens.Lens' DeleteMultiplexProgramResponse (Lude.Maybe MultiplexProgramSettings)
dmprsMultiplexProgramSettings = Lens.lens (multiplexProgramSettings :: DeleteMultiplexProgramResponse -> Lude.Maybe MultiplexProgramSettings) (\s a -> s {multiplexProgramSettings = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsResponseStatus :: Lens.Lens' DeleteMultiplexProgramResponse Lude.Int
dmprsResponseStatus = Lens.lens (responseStatus :: DeleteMultiplexProgramResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMultiplexProgramResponse)
{-# DEPRECATED dmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

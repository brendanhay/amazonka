{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new program in the multiplex.
module Network.AWS.MediaLive.CreateMultiplexProgram
  ( -- * Creating a request
    CreateMultiplexProgram (..),
    mkCreateMultiplexProgram,

    -- ** Request lenses
    cmpMultiplexId,
    cmpRequestId,
    cmpMultiplexProgramSettings,
    cmpProgramName,

    -- * Destructuring the response
    CreateMultiplexProgramResponse (..),
    mkCreateMultiplexProgramResponse,

    -- ** Response lenses
    cmprsMultiplexProgram,
    cmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to create a program in a multiplex.
--
-- /See:/ 'mkCreateMultiplexProgram' smart constructor.
data CreateMultiplexProgram = CreateMultiplexProgram'
  { multiplexId ::
      Lude.Text,
    requestId :: Lude.Text,
    multiplexProgramSettings ::
      MultiplexProgramSettings,
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

-- | Creates a value of 'CreateMultiplexProgram' with the minimum fields required to make a request.
--
-- * 'multiplexId' - ID of the multiplex where the program is to be created.
-- * 'multiplexProgramSettings' - The settings for this multiplex program.
-- * 'programName' - Name of multiplex program.
-- * 'requestId' - Unique request ID. This prevents retries from creating multiple
--
-- resources.
mkCreateMultiplexProgram ::
  -- | 'multiplexId'
  Lude.Text ->
  -- | 'requestId'
  Lude.Text ->
  -- | 'multiplexProgramSettings'
  MultiplexProgramSettings ->
  -- | 'programName'
  Lude.Text ->
  CreateMultiplexProgram
mkCreateMultiplexProgram
  pMultiplexId_
  pRequestId_
  pMultiplexProgramSettings_
  pProgramName_ =
    CreateMultiplexProgram'
      { multiplexId = pMultiplexId_,
        requestId = pRequestId_,
        multiplexProgramSettings = pMultiplexProgramSettings_,
        programName = pProgramName_
      }

-- | ID of the multiplex where the program is to be created.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexId :: Lens.Lens' CreateMultiplexProgram Lude.Text
cmpMultiplexId = Lens.lens (multiplexId :: CreateMultiplexProgram -> Lude.Text) (\s a -> s {multiplexId = a} :: CreateMultiplexProgram)
{-# DEPRECATED cmpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpRequestId :: Lens.Lens' CreateMultiplexProgram Lude.Text
cmpRequestId = Lens.lens (requestId :: CreateMultiplexProgram -> Lude.Text) (\s a -> s {requestId = a} :: CreateMultiplexProgram)
{-# DEPRECATED cmpRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexProgramSettings :: Lens.Lens' CreateMultiplexProgram MultiplexProgramSettings
cmpMultiplexProgramSettings = Lens.lens (multiplexProgramSettings :: CreateMultiplexProgram -> MultiplexProgramSettings) (\s a -> s {multiplexProgramSettings = a} :: CreateMultiplexProgram)
{-# DEPRECATED cmpMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | Name of multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpProgramName :: Lens.Lens' CreateMultiplexProgram Lude.Text
cmpProgramName = Lens.lens (programName :: CreateMultiplexProgram -> Lude.Text) (\s a -> s {programName = a} :: CreateMultiplexProgram)
{-# DEPRECATED cmpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Lude.AWSRequest CreateMultiplexProgram where
  type Rs CreateMultiplexProgram = CreateMultiplexProgramResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMultiplexProgramResponse'
            Lude.<$> (x Lude..?> "multiplexProgram")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMultiplexProgram where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMultiplexProgram where
  toJSON CreateMultiplexProgram' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("requestId" Lude..= requestId),
            Lude.Just
              ("multiplexProgramSettings" Lude..= multiplexProgramSettings),
            Lude.Just ("programName" Lude..= programName)
          ]
      )

instance Lude.ToPath CreateMultiplexProgram where
  toPath CreateMultiplexProgram' {..} =
    Lude.mconcat
      ["/prod/multiplexes/", Lude.toBS multiplexId, "/programs"]

instance Lude.ToQuery CreateMultiplexProgram where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CreateMultiplexProgramResponse
--
-- /See:/ 'mkCreateMultiplexProgramResponse' smart constructor.
data CreateMultiplexProgramResponse = CreateMultiplexProgramResponse'
  { multiplexProgram ::
      Lude.Maybe MultiplexProgram,
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

-- | Creates a value of 'CreateMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- * 'multiplexProgram' - The newly created multiplex program.
-- * 'responseStatus' - The response status code.
mkCreateMultiplexProgramResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMultiplexProgramResponse
mkCreateMultiplexProgramResponse pResponseStatus_ =
  CreateMultiplexProgramResponse'
    { multiplexProgram = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprsMultiplexProgram :: Lens.Lens' CreateMultiplexProgramResponse (Lude.Maybe MultiplexProgram)
cmprsMultiplexProgram = Lens.lens (multiplexProgram :: CreateMultiplexProgramResponse -> Lude.Maybe MultiplexProgram) (\s a -> s {multiplexProgram = a} :: CreateMultiplexProgramResponse)
{-# DEPRECATED cmprsMultiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprsResponseStatus :: Lens.Lens' CreateMultiplexProgramResponse Lude.Int
cmprsResponseStatus = Lens.lens (responseStatus :: CreateMultiplexProgramResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMultiplexProgramResponse)
{-# DEPRECATED cmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

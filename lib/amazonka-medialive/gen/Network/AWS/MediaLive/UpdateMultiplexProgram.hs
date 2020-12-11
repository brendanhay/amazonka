{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a program in a multiplex.
module Network.AWS.MediaLive.UpdateMultiplexProgram
  ( -- * Creating a request
    UpdateMultiplexProgram (..),
    mkUpdateMultiplexProgram,

    -- ** Request lenses
    umpMultiplexProgramSettings,
    umpMultiplexId,
    umpProgramName,

    -- * Destructuring the response
    UpdateMultiplexProgramResponse (..),
    mkUpdateMultiplexProgramResponse,

    -- ** Response lenses
    umprsMultiplexProgram,
    umprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update a program in a multiplex.
--
-- /See:/ 'mkUpdateMultiplexProgram' smart constructor.
data UpdateMultiplexProgram = UpdateMultiplexProgram'
  { multiplexProgramSettings ::
      Lude.Maybe MultiplexProgramSettings,
    multiplexId :: Lude.Text,
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

-- | Creates a value of 'UpdateMultiplexProgram' with the minimum fields required to make a request.
--
-- * 'multiplexId' - The ID of the multiplex of the program to update.
-- * 'multiplexProgramSettings' - The new settings for a multiplex program.
-- * 'programName' - The name of the program to update.
mkUpdateMultiplexProgram ::
  -- | 'multiplexId'
  Lude.Text ->
  -- | 'programName'
  Lude.Text ->
  UpdateMultiplexProgram
mkUpdateMultiplexProgram pMultiplexId_ pProgramName_ =
  UpdateMultiplexProgram'
    { multiplexProgramSettings = Lude.Nothing,
      multiplexId = pMultiplexId_,
      programName = pProgramName_
    }

-- | The new settings for a multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexProgramSettings :: Lens.Lens' UpdateMultiplexProgram (Lude.Maybe MultiplexProgramSettings)
umpMultiplexProgramSettings = Lens.lens (multiplexProgramSettings :: UpdateMultiplexProgram -> Lude.Maybe MultiplexProgramSettings) (\s a -> s {multiplexProgramSettings = a} :: UpdateMultiplexProgram)
{-# DEPRECATED umpMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | The ID of the multiplex of the program to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexId :: Lens.Lens' UpdateMultiplexProgram Lude.Text
umpMultiplexId = Lens.lens (multiplexId :: UpdateMultiplexProgram -> Lude.Text) (\s a -> s {multiplexId = a} :: UpdateMultiplexProgram)
{-# DEPRECATED umpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The name of the program to update.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpProgramName :: Lens.Lens' UpdateMultiplexProgram Lude.Text
umpProgramName = Lens.lens (programName :: UpdateMultiplexProgram -> Lude.Text) (\s a -> s {programName = a} :: UpdateMultiplexProgram)
{-# DEPRECATED umpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Lude.AWSRequest UpdateMultiplexProgram where
  type Rs UpdateMultiplexProgram = UpdateMultiplexProgramResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMultiplexProgramResponse'
            Lude.<$> (x Lude..?> "multiplexProgram")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMultiplexProgram where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMultiplexProgram where
  toJSON UpdateMultiplexProgram' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("multiplexProgramSettings" Lude..=)
              Lude.<$> multiplexProgramSettings
          ]
      )

instance Lude.ToPath UpdateMultiplexProgram where
  toPath UpdateMultiplexProgram' {..} =
    Lude.mconcat
      [ "/prod/multiplexes/",
        Lude.toBS multiplexId,
        "/programs/",
        Lude.toBS programName
      ]

instance Lude.ToQuery UpdateMultiplexProgram where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateMultiplexProgramResponse
--
-- /See:/ 'mkUpdateMultiplexProgramResponse' smart constructor.
data UpdateMultiplexProgramResponse = UpdateMultiplexProgramResponse'
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

-- | Creates a value of 'UpdateMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- * 'multiplexProgram' - The updated multiplex program.
-- * 'responseStatus' - The response status code.
mkUpdateMultiplexProgramResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMultiplexProgramResponse
mkUpdateMultiplexProgramResponse pResponseStatus_ =
  UpdateMultiplexProgramResponse'
    { multiplexProgram = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprsMultiplexProgram :: Lens.Lens' UpdateMultiplexProgramResponse (Lude.Maybe MultiplexProgram)
umprsMultiplexProgram = Lens.lens (multiplexProgram :: UpdateMultiplexProgramResponse -> Lude.Maybe MultiplexProgram) (\s a -> s {multiplexProgram = a} :: UpdateMultiplexProgramResponse)
{-# DEPRECATED umprsMultiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprsResponseStatus :: Lens.Lens' UpdateMultiplexProgramResponse Lude.Int
umprsResponseStatus = Lens.lens (responseStatus :: UpdateMultiplexProgramResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMultiplexProgramResponse)
{-# DEPRECATED umprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing stream. The stream version will be incremented by one.
module Network.AWS.IoT.UpdateStream
  ( -- * Creating a request
    UpdateStream (..),
    mkUpdateStream,

    -- ** Request lenses
    usFiles,
    usDescription,
    usRoleARN,
    usStreamId,

    -- * Destructuring the response
    UpdateStreamResponse (..),
    mkUpdateStreamResponse,

    -- ** Response lenses
    usrsStreamVersion,
    usrsStreamARN,
    usrsDescription,
    usrsStreamId,
    usrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { files ::
      Lude.Maybe (Lude.NonEmpty StreamFile),
    description :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    streamId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStream' with the minimum fields required to make a request.
--
-- * 'description' - The description of the stream.
-- * 'files' - The files associated with the stream.
-- * 'roleARN' - An IAM role that allows the IoT service principal assumes to access your S3 files.
-- * 'streamId' - The stream ID.
mkUpdateStream ::
  -- | 'streamId'
  Lude.Text ->
  UpdateStream
mkUpdateStream pStreamId_ =
  UpdateStream'
    { files = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing,
      streamId = pStreamId_
    }

-- | The files associated with the stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usFiles :: Lens.Lens' UpdateStream (Lude.Maybe (Lude.NonEmpty StreamFile))
usFiles = Lens.lens (files :: UpdateStream -> Lude.Maybe (Lude.NonEmpty StreamFile)) (\s a -> s {files = a} :: UpdateStream)
{-# DEPRECATED usFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | The description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
usDescription = Lens.lens (description :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateStream)
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRoleARN :: Lens.Lens' UpdateStream (Lude.Maybe Lude.Text)
usRoleARN = Lens.lens (roleARN :: UpdateStream -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateStream)
{-# DEPRECATED usRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamId :: Lens.Lens' UpdateStream Lude.Text
usStreamId = Lens.lens (streamId :: UpdateStream -> Lude.Text) (\s a -> s {streamId = a} :: UpdateStream)
{-# DEPRECATED usStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

instance Lude.AWSRequest UpdateStream where
  type Rs UpdateStream = UpdateStreamResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateStreamResponse'
            Lude.<$> (x Lude..?> "streamVersion")
            Lude.<*> (x Lude..?> "streamArn")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "streamId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateStream where
  toJSON UpdateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("files" Lude..=) Lude.<$> files,
            ("description" Lude..=) Lude.<$> description,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateStream where
  toPath UpdateStream' {..} =
    Lude.mconcat ["/streams/", Lude.toBS streamId]

instance Lude.ToQuery UpdateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStreamResponse' smart constructor.
data UpdateStreamResponse = UpdateStreamResponse'
  { streamVersion ::
      Lude.Maybe Lude.Natural,
    streamARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    streamId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateStreamResponse' with the minimum fields required to make a request.
--
-- * 'description' - A description of the stream.
-- * 'responseStatus' - The response status code.
-- * 'streamARN' - The stream ARN.
-- * 'streamId' - The stream ID.
-- * 'streamVersion' - The stream version.
mkUpdateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStreamResponse
mkUpdateStreamResponse pResponseStatus_ =
  UpdateStreamResponse'
    { streamVersion = Lude.Nothing,
      streamARN = Lude.Nothing,
      description = Lude.Nothing,
      streamId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsStreamVersion :: Lens.Lens' UpdateStreamResponse (Lude.Maybe Lude.Natural)
usrsStreamVersion = Lens.lens (streamVersion :: UpdateStreamResponse -> Lude.Maybe Lude.Natural) (\s a -> s {streamVersion = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsStreamARN :: Lens.Lens' UpdateStreamResponse (Lude.Maybe Lude.Text)
usrsStreamARN = Lens.lens (streamARN :: UpdateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsDescription :: Lens.Lens' UpdateStreamResponse (Lude.Maybe Lude.Text)
usrsDescription = Lens.lens (description :: UpdateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsStreamId :: Lens.Lens' UpdateStreamResponse (Lude.Maybe Lude.Text)
usrsStreamId = Lens.lens (streamId :: UpdateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamId = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateStreamResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStreamResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

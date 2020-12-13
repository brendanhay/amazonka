{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream.
module Network.AWS.IoT.CreateStream
  ( -- * Creating a request
    CreateStream (..),
    mkCreateStream,

    -- ** Request lenses
    csFiles,
    csDescription,
    csStreamId,
    csTags,
    csRoleARN,

    -- * Destructuring the response
    CreateStreamResponse (..),
    mkCreateStreamResponse,

    -- ** Response lenses
    csrsStreamVersion,
    csrsStreamARN,
    csrsDescription,
    csrsStreamId,
    csrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | The files to stream.
    files :: Lude.NonEmpty StreamFile,
    -- | A description of the stream.
    description :: Lude.Maybe Lude.Text,
    -- | The stream ID.
    streamId :: Lude.Text,
    -- | Metadata which can be used to manage streams.
    tags :: Lude.Maybe [Tag],
    -- | An IAM role that allows the IoT service principal assumes to access your S3 files.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- * 'files' - The files to stream.
-- * 'description' - A description of the stream.
-- * 'streamId' - The stream ID.
-- * 'tags' - Metadata which can be used to manage streams.
-- * 'roleARN' - An IAM role that allows the IoT service principal assumes to access your S3 files.
mkCreateStream ::
  -- | 'files'
  Lude.NonEmpty StreamFile ->
  -- | 'streamId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateStream
mkCreateStream pFiles_ pStreamId_ pRoleARN_ =
  CreateStream'
    { files = pFiles_,
      description = Lude.Nothing,
      streamId = pStreamId_,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The files to stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csFiles :: Lens.Lens' CreateStream (Lude.NonEmpty StreamFile)
csFiles = Lens.lens (files :: CreateStream -> Lude.NonEmpty StreamFile) (\s a -> s {files = a} :: CreateStream)
{-# DEPRECATED csFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateStream (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateStream -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStream)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamId :: Lens.Lens' CreateStream Lude.Text
csStreamId = Lens.lens (streamId :: CreateStream -> Lude.Text) (\s a -> s {streamId = a} :: CreateStream)
{-# DEPRECATED csStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | Metadata which can be used to manage streams.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStream (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateStream -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStream)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | An IAM role that allows the IoT service principal assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' CreateStream Lude.Text
csRoleARN = Lens.lens (roleARN :: CreateStream -> Lude.Text) (\s a -> s {roleARN = a} :: CreateStream)
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateStream where
  type Rs CreateStream = CreateStreamResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStreamResponse'
            Lude.<$> (x Lude..?> "streamVersion")
            Lude.<*> (x Lude..?> "streamArn")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "streamId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("files" Lude..= files),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateStream where
  toPath CreateStream' {..} =
    Lude.mconcat ["/streams/", Lude.toBS streamId]

instance Lude.ToQuery CreateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { -- | The version of the stream.
    streamVersion :: Lude.Maybe Lude.Natural,
    -- | The stream ARN.
    streamARN :: Lude.Maybe Lude.Text,
    -- | A description of the stream.
    description :: Lude.Maybe Lude.Text,
    -- | The stream ID.
    streamId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
--
-- * 'streamVersion' - The version of the stream.
-- * 'streamARN' - The stream ARN.
-- * 'description' - A description of the stream.
-- * 'streamId' - The stream ID.
-- * 'responseStatus' - The response status code.
mkCreateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamResponse
mkCreateStreamResponse pResponseStatus_ =
  CreateStreamResponse'
    { streamVersion = Lude.Nothing,
      streamARN = Lude.Nothing,
      description = Lude.Nothing,
      streamId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version of the stream.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStreamVersion :: Lens.Lens' CreateStreamResponse (Lude.Maybe Lude.Natural)
csrsStreamVersion = Lens.lens (streamVersion :: CreateStreamResponse -> Lude.Maybe Lude.Natural) (\s a -> s {streamVersion = a} :: CreateStreamResponse)
{-# DEPRECATED csrsStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStreamARN :: Lens.Lens' CreateStreamResponse (Lude.Maybe Lude.Text)
csrsStreamARN = Lens.lens (streamARN :: CreateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: CreateStreamResponse)
{-# DEPRECATED csrsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsDescription :: Lens.Lens' CreateStreamResponse (Lude.Maybe Lude.Text)
csrsDescription = Lens.lens (description :: CreateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStreamResponse)
{-# DEPRECATED csrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStreamId :: Lens.Lens' CreateStreamResponse (Lude.Maybe Lude.Text)
csrsStreamId = Lens.lens (streamId :: CreateStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamId = a} :: CreateStreamResponse)
{-# DEPRECATED csrsStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateStreamResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

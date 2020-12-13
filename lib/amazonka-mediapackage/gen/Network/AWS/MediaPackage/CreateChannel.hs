{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Channel.
module Network.AWS.MediaPackage.CreateChannel
  ( -- * Creating a request
    CreateChannel (..),
    mkCreateChannel,

    -- ** Request lenses
    ccId,
    ccDescription,
    ccTags,

    -- * Destructuring the response
    CreateChannelResponse (..),
    mkCreateChannelResponse,

    -- ** Response lenses
    ccrsIngressAccessLogs,
    ccrsHlsIngest,
    ccrsARN,
    ccrsId,
    ccrsDescription,
    ccrsEgressAccessLogs,
    ccrsTags,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A new Channel configuration.
--
-- /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | The ID of the Channel. The ID must be unique within the region and it
    --
    -- cannot be changed after a Channel is created.
    id :: Lude.Text,
    -- | A short text description of the Channel.
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the Channel. The ID must be unique within the region and it
--
-- cannot be changed after a Channel is created.
-- * 'description' - A short text description of the Channel.
-- * 'tags' -
mkCreateChannel ::
  -- | 'id'
  Lude.Text ->
  CreateChannel
mkCreateChannel pId_ =
  CreateChannel'
    { id = pId_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the Channel. The ID must be unique within the region and it
--
-- cannot be changed after a Channel is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccId :: Lens.Lens' CreateChannel Lude.Text
ccId = Lens.lens (id :: CreateChannel -> Lude.Text) (\s a -> s {id = a} :: CreateChannel)
{-# DEPRECATED ccId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateChannel (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: CreateChannel -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateChannel)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccTags = Lens.lens (tags :: CreateChannel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateChannel)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = Req.postJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Lude.<$> (x Lude..?> "ingressAccessLogs")
            Lude.<*> (x Lude..?> "hlsIngest")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "egressAccessLogs")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("id" Lude..= id),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateChannel where
  toPath = Lude.const "/channels"

instance Lude.ToQuery CreateChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { ingressAccessLogs :: Lude.Maybe IngressAccessLogs,
    hlsIngest :: Lude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID of the Channel.
    id :: Lude.Maybe Lude.Text,
    -- | A short text description of the Channel.
    description :: Lude.Maybe Lude.Text,
    egressAccessLogs :: Lude.Maybe EgressAccessLogs,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- * 'ingressAccessLogs' -
-- * 'hlsIngest' -
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'id' - The ID of the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' -
-- * 'tags' -
-- * 'responseStatus' - The response status code.
mkCreateChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateChannelResponse
mkCreateChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { ingressAccessLogs = Lude.Nothing,
      hlsIngest = Lude.Nothing,
      arn = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      egressAccessLogs = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsIngressAccessLogs :: Lens.Lens' CreateChannelResponse (Lude.Maybe IngressAccessLogs)
ccrsIngressAccessLogs = Lens.lens (ingressAccessLogs :: CreateChannelResponse -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsHlsIngest :: Lens.Lens' CreateChannelResponse (Lude.Maybe HlsIngest)
ccrsHlsIngest = Lens.lens (hlsIngest :: CreateChannelResponse -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsARN :: Lens.Lens' CreateChannelResponse (Lude.Maybe Lude.Text)
ccrsARN = Lens.lens (arn :: CreateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsId :: Lens.Lens' CreateChannelResponse (Lude.Maybe Lude.Text)
ccrsId = Lens.lens (id :: CreateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsDescription :: Lens.Lens' CreateChannelResponse (Lude.Maybe Lude.Text)
ccrsDescription = Lens.lens (description :: CreateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsEgressAccessLogs :: Lens.Lens' CreateChannelResponse (Lude.Maybe EgressAccessLogs)
ccrsEgressAccessLogs = Lens.lens (egressAccessLogs :: CreateChannelResponse -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsTags :: Lens.Lens' CreateChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccrsTags = Lens.lens (tags :: CreateChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateChannelResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateChannelResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Network.AWS.MediaLive.CreateMultiplex
  ( -- * Creating a request
    CreateMultiplex (..),
    mkCreateMultiplex,

    -- ** Request lenses
    cmRequestId,
    cmAvailabilityZones,
    cmName,
    cmMultiplexSettings,
    cmTags,

    -- * Destructuring the response
    CreateMultiplexResponse (..),
    mkCreateMultiplexResponse,

    -- ** Response lenses
    cmrsMultiplex,
    cmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to create a multiplex.
--
-- /See:/ 'mkCreateMultiplex' smart constructor.
data CreateMultiplex = CreateMultiplex'
  { -- | Unique request ID. This prevents retries from creating multiple
    --
    -- resources.
    requestId :: Lude.Text,
    -- | A list of availability zones for the multiplex. You must specify exactly two.
    availabilityZones :: [Lude.Text],
    -- | Name of multiplex.
    name :: Lude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: MultiplexSettings,
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMultiplex' with the minimum fields required to make a request.
--
-- * 'requestId' - Unique request ID. This prevents retries from creating multiple
--
-- resources.
-- * 'availabilityZones' - A list of availability zones for the multiplex. You must specify exactly two.
-- * 'name' - Name of multiplex.
-- * 'multiplexSettings' - Configuration for a multiplex event.
-- * 'tags' - A collection of key-value pairs.
mkCreateMultiplex ::
  -- | 'requestId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'multiplexSettings'
  MultiplexSettings ->
  CreateMultiplex
mkCreateMultiplex pRequestId_ pName_ pMultiplexSettings_ =
  CreateMultiplex'
    { requestId = pRequestId_,
      availabilityZones = Lude.mempty,
      name = pName_,
      multiplexSettings = pMultiplexSettings_,
      tags = Lude.Nothing
    }

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRequestId :: Lens.Lens' CreateMultiplex Lude.Text
cmRequestId = Lens.lens (requestId :: CreateMultiplex -> Lude.Text) (\s a -> s {requestId = a} :: CreateMultiplex)
{-# DEPRECATED cmRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | A list of availability zones for the multiplex. You must specify exactly two.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAvailabilityZones :: Lens.Lens' CreateMultiplex [Lude.Text]
cmAvailabilityZones = Lens.lens (availabilityZones :: CreateMultiplex -> [Lude.Text]) (\s a -> s {availabilityZones = a} :: CreateMultiplex)
{-# DEPRECATED cmAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Name of multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CreateMultiplex Lude.Text
cmName = Lens.lens (name :: CreateMultiplex -> Lude.Text) (\s a -> s {name = a} :: CreateMultiplex)
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMultiplexSettings :: Lens.Lens' CreateMultiplex MultiplexSettings
cmMultiplexSettings = Lens.lens (multiplexSettings :: CreateMultiplex -> MultiplexSettings) (\s a -> s {multiplexSettings = a} :: CreateMultiplex)
{-# DEPRECATED cmMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTags :: Lens.Lens' CreateMultiplex (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cmTags = Lens.lens (tags :: CreateMultiplex -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateMultiplex)
{-# DEPRECATED cmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateMultiplex where
  type Rs CreateMultiplex = CreateMultiplexResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMultiplexResponse'
            Lude.<$> (x Lude..?> "multiplex") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMultiplex where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMultiplex where
  toJSON CreateMultiplex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("requestId" Lude..= requestId),
            Lude.Just ("availabilityZones" Lude..= availabilityZones),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("multiplexSettings" Lude..= multiplexSettings),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateMultiplex where
  toPath = Lude.const "/prod/multiplexes"

instance Lude.ToQuery CreateMultiplex where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'mkCreateMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { -- | The newly created multiplex.
    multiplex :: Lude.Maybe Multiplex,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMultiplexResponse' with the minimum fields required to make a request.
--
-- * 'multiplex' - The newly created multiplex.
-- * 'responseStatus' - The response status code.
mkCreateMultiplexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMultiplexResponse
mkCreateMultiplexResponse pResponseStatus_ =
  CreateMultiplexResponse'
    { multiplex = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsMultiplex :: Lens.Lens' CreateMultiplexResponse (Lude.Maybe Multiplex)
cmrsMultiplex = Lens.lens (multiplex :: CreateMultiplexResponse -> Lude.Maybe Multiplex) (\s a -> s {multiplex = a} :: CreateMultiplexResponse)
{-# DEPRECATED cmrsMultiplex "Use generic-lens or generic-optics with 'multiplex' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsResponseStatus :: Lens.Lens' CreateMultiplexResponse Lude.Int
cmrsResponseStatus = Lens.lens (responseStatus :: CreateMultiplexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMultiplexResponse)
{-# DEPRECATED cmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerAPIMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
module Network.AWS.Lightsail.GetContainerAPIMetadata
  ( -- * Creating a request
    GetContainerAPIMetadata (..),
    mkGetContainerAPIMetadata,

    -- * Destructuring the response
    GetContainerAPIMetadataResponse (..),
    mkGetContainerAPIMetadataResponse,

    -- ** Response lenses
    gcamrsMetadata,
    gcamrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerAPIMetadata' smart constructor.
data GetContainerAPIMetadata = GetContainerAPIMetadata'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerAPIMetadata' with the minimum fields required to make a request.
mkGetContainerAPIMetadata ::
  GetContainerAPIMetadata
mkGetContainerAPIMetadata = GetContainerAPIMetadata'

instance Lude.AWSRequest GetContainerAPIMetadata where
  type Rs GetContainerAPIMetadata = GetContainerAPIMetadataResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerAPIMetadataResponse'
            Lude.<$> (x Lude..?> "metadata" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerAPIMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetContainerAPIMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerAPIMetadata where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetContainerAPIMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerAPIMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerAPIMetadataResponse' smart constructor.
data GetContainerAPIMetadataResponse = GetContainerAPIMetadataResponse'
  { metadata ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (Lude.Text)
        ],
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

-- | Creates a value of 'GetContainerAPIMetadataResponse' with the minimum fields required to make a request.
--
-- * 'metadata' - Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
-- * 'responseStatus' - The response status code.
mkGetContainerAPIMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerAPIMetadataResponse
mkGetContainerAPIMetadataResponse pResponseStatus_ =
  GetContainerAPIMetadataResponse'
    { metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcamrsMetadata :: Lens.Lens' GetContainerAPIMetadataResponse (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
gcamrsMetadata = Lens.lens (metadata :: GetContainerAPIMetadataResponse -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {metadata = a} :: GetContainerAPIMetadataResponse)
{-# DEPRECATED gcamrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcamrsResponseStatus :: Lens.Lens' GetContainerAPIMetadataResponse Lude.Int
gcamrsResponseStatus = Lens.lens (responseStatus :: GetContainerAPIMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerAPIMetadataResponse)
{-# DEPRECATED gcamrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

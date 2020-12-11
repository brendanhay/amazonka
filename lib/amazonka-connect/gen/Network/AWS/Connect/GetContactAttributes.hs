{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetContactAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contact attributes for the specified contact.
module Network.AWS.Connect.GetContactAttributes
  ( -- * Creating a request
    GetContactAttributes (..),
    mkGetContactAttributes,

    -- ** Request lenses
    gcaInstanceId,
    gcaInitialContactId,

    -- * Destructuring the response
    GetContactAttributesResponse (..),
    mkGetContactAttributesResponse,

    -- ** Response lenses
    gcarsAttributes,
    gcarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContactAttributes' smart constructor.
data GetContactAttributes = GetContactAttributes'
  { instanceId ::
      Lude.Text,
    initialContactId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContactAttributes' with the minimum fields required to make a request.
--
-- * 'initialContactId' - The identifier of the initial contact.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkGetContactAttributes ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'initialContactId'
  Lude.Text ->
  GetContactAttributes
mkGetContactAttributes pInstanceId_ pInitialContactId_ =
  GetContactAttributes'
    { instanceId = pInstanceId_,
      initialContactId = pInitialContactId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaInstanceId :: Lens.Lens' GetContactAttributes Lude.Text
gcaInstanceId = Lens.lens (instanceId :: GetContactAttributes -> Lude.Text) (\s a -> s {instanceId = a} :: GetContactAttributes)
{-# DEPRECATED gcaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the initial contact.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaInitialContactId :: Lens.Lens' GetContactAttributes Lude.Text
gcaInitialContactId = Lens.lens (initialContactId :: GetContactAttributes -> Lude.Text) (\s a -> s {initialContactId = a} :: GetContactAttributes)
{-# DEPRECATED gcaInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Lude.AWSRequest GetContactAttributes where
  type Rs GetContactAttributes = GetContactAttributesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContactAttributesResponse'
            Lude.<$> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContactAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetContactAttributes where
  toPath GetContactAttributes' {..} =
    Lude.mconcat
      [ "/contact/attributes/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS initialContactId
      ]

instance Lude.ToQuery GetContactAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContactAttributesResponse' smart constructor.
data GetContactAttributesResponse = GetContactAttributesResponse'
  { attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'GetContactAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Information about the attributes.
-- * 'responseStatus' - The response status code.
mkGetContactAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContactAttributesResponse
mkGetContactAttributesResponse pResponseStatus_ =
  GetContactAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarsAttributes :: Lens.Lens' GetContactAttributesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcarsAttributes = Lens.lens (attributes :: GetContactAttributesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: GetContactAttributesResponse)
{-# DEPRECATED gcarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarsResponseStatus :: Lens.Lens' GetContactAttributesResponse Lude.Int
gcarsResponseStatus = Lens.lens (responseStatus :: GetContactAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContactAttributesResponse)
{-# DEPRECATED gcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

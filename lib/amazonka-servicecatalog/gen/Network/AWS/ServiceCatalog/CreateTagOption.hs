{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TagOption.
module Network.AWS.ServiceCatalog.CreateTagOption
  ( -- * Creating a request
    CreateTagOption (..),
    mkCreateTagOption,

    -- ** Request lenses
    ctoValue,
    ctoKey,

    -- * Destructuring the response
    CreateTagOptionResponse (..),
    mkCreateTagOptionResponse,

    -- ** Response lenses
    ctorsTagOptionDetail,
    ctorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreateTagOption' smart constructor.
data CreateTagOption = CreateTagOption'
  { -- | The TagOption value.
    value :: Lude.Text,
    -- | The TagOption key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagOption' with the minimum fields required to make a request.
--
-- * 'value' - The TagOption value.
-- * 'key' - The TagOption key.
mkCreateTagOption ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  CreateTagOption
mkCreateTagOption pValue_ pKey_ =
  CreateTagOption' {value = pValue_, key = pKey_}

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoValue :: Lens.Lens' CreateTagOption Lude.Text
ctoValue = Lens.lens (value :: CreateTagOption -> Lude.Text) (\s a -> s {value = a} :: CreateTagOption)
{-# DEPRECATED ctoValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoKey :: Lens.Lens' CreateTagOption Lude.Text
ctoKey = Lens.lens (key :: CreateTagOption -> Lude.Text) (\s a -> s {key = a} :: CreateTagOption)
{-# DEPRECATED ctoKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest CreateTagOption where
  type Rs CreateTagOption = CreateTagOptionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTagOptionResponse'
            Lude.<$> (x Lude..?> "TagOptionDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTagOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.CreateTagOption" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTagOption where
  toJSON CreateTagOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Value" Lude..= value), Lude.Just ("Key" Lude..= key)]
      )

instance Lude.ToPath CreateTagOption where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTagOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTagOptionResponse' smart constructor.
data CreateTagOptionResponse = CreateTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Lude.Maybe TagOptionDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagOptionResponse' with the minimum fields required to make a request.
--
-- * 'tagOptionDetail' - Information about the TagOption.
-- * 'responseStatus' - The response status code.
mkCreateTagOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTagOptionResponse
mkCreateTagOptionResponse pResponseStatus_ =
  CreateTagOptionResponse'
    { tagOptionDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorsTagOptionDetail :: Lens.Lens' CreateTagOptionResponse (Lude.Maybe TagOptionDetail)
ctorsTagOptionDetail = Lens.lens (tagOptionDetail :: CreateTagOptionResponse -> Lude.Maybe TagOptionDetail) (\s a -> s {tagOptionDetail = a} :: CreateTagOptionResponse)
{-# DEPRECATED ctorsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorsResponseStatus :: Lens.Lens' CreateTagOptionResponse Lude.Int
ctorsResponseStatus = Lens.lens (responseStatus :: CreateTagOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTagOptionResponse)
{-# DEPRECATED ctorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

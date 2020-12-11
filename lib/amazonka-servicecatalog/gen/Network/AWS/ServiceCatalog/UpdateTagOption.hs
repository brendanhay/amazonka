{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified TagOption.
module Network.AWS.ServiceCatalog.UpdateTagOption
  ( -- * Creating a request
    UpdateTagOption (..),
    mkUpdateTagOption,

    -- ** Request lenses
    utoValue,
    utoActive,
    utoId,

    -- * Destructuring the response
    UpdateTagOptionResponse (..),
    mkUpdateTagOptionResponse,

    -- ** Response lenses
    utorsTagOptionDetail,
    utorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { value ::
      Lude.Maybe Lude.Text,
    active :: Lude.Maybe Lude.Bool,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTagOption' with the minimum fields required to make a request.
--
-- * 'active' - The updated active state.
-- * 'id' - The TagOption identifier.
-- * 'value' - The updated value.
mkUpdateTagOption ::
  -- | 'id'
  Lude.Text ->
  UpdateTagOption
mkUpdateTagOption pId_ =
  UpdateTagOption'
    { value = Lude.Nothing,
      active = Lude.Nothing,
      id = pId_
    }

-- | The updated value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoValue :: Lens.Lens' UpdateTagOption (Lude.Maybe Lude.Text)
utoValue = Lens.lens (value :: UpdateTagOption -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: UpdateTagOption)
{-# DEPRECATED utoValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The updated active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoActive :: Lens.Lens' UpdateTagOption (Lude.Maybe Lude.Bool)
utoActive = Lens.lens (active :: UpdateTagOption -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: UpdateTagOption)
{-# DEPRECATED utoActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoId :: Lens.Lens' UpdateTagOption Lude.Text
utoId = Lens.lens (id :: UpdateTagOption -> Lude.Text) (\s a -> s {id = a} :: UpdateTagOption)
{-# DEPRECATED utoId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateTagOption where
  type Rs UpdateTagOption = UpdateTagOptionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTagOptionResponse'
            Lude.<$> (x Lude..?> "TagOptionDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTagOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.UpdateTagOption" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTagOption where
  toJSON UpdateTagOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Active" Lude..=) Lude.<$> active,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdateTagOption where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTagOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { tagOptionDetail ::
      Lude.Maybe TagOptionDetail,
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

-- | Creates a value of 'UpdateTagOptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tagOptionDetail' - Information about the TagOption.
mkUpdateTagOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTagOptionResponse
mkUpdateTagOptionResponse pResponseStatus_ =
  UpdateTagOptionResponse'
    { tagOptionDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorsTagOptionDetail :: Lens.Lens' UpdateTagOptionResponse (Lude.Maybe TagOptionDetail)
utorsTagOptionDetail = Lens.lens (tagOptionDetail :: UpdateTagOptionResponse -> Lude.Maybe TagOptionDetail) (\s a -> s {tagOptionDetail = a} :: UpdateTagOptionResponse)
{-# DEPRECATED utorsTagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorsResponseStatus :: Lens.Lens' UpdateTagOptionResponse Lude.Int
utorsResponseStatus = Lens.lens (responseStatus :: UpdateTagOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTagOptionResponse)
{-# DEPRECATED utorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

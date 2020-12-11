{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connection definition in the Data Catalog.
module Network.AWS.Glue.UpdateConnection
  ( -- * Creating a request
    UpdateConnection (..),
    mkUpdateConnection,

    -- ** Request lenses
    ucCatalogId,
    ucName,
    ucConnectionInput,

    -- * Destructuring the response
    UpdateConnectionResponse (..),
    mkUpdateConnectionResponse,

    -- ** Response lenses
    ucrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { catalogId ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    connectionInput :: ConnectionInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConnection' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
-- * 'connectionInput' - A @ConnectionInput@ object that redefines the connection in question.
-- * 'name' - The name of the connection definition to update.
mkUpdateConnection ::
  -- | 'name'
  Lude.Text ->
  -- | 'connectionInput'
  ConnectionInput ->
  UpdateConnection
mkUpdateConnection pName_ pConnectionInput_ =
  UpdateConnection'
    { catalogId = Lude.Nothing,
      name = pName_,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCatalogId :: Lens.Lens' UpdateConnection (Lude.Maybe Lude.Text)
ucCatalogId = Lens.lens (catalogId :: UpdateConnection -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UpdateConnection)
{-# DEPRECATED ucCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the connection definition to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateConnection Lude.Text
ucName = Lens.lens (name :: UpdateConnection -> Lude.Text) (\s a -> s {name = a} :: UpdateConnection)
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A @ConnectionInput@ object that redefines the connection in question.
--
-- /Note:/ Consider using 'connectionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucConnectionInput :: Lens.Lens' UpdateConnection ConnectionInput
ucConnectionInput = Lens.lens (connectionInput :: UpdateConnection -> ConnectionInput) (\s a -> s {connectionInput = a} :: UpdateConnection)
{-# DEPRECATED ucConnectionInput "Use generic-lens or generic-optics with 'connectionInput' instead." #-}

instance Lude.AWSRequest UpdateConnection where
  type Rs UpdateConnection = UpdateConnectionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateConnectionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("ConnectionInput" Lude..= connectionInput)
          ]
      )

instance Lude.ToPath UpdateConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConnectionResponse' smart constructor.
newtype UpdateConnectionResponse = UpdateConnectionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConnectionResponse
mkUpdateConnectionResponse pResponseStatus_ =
  UpdateConnectionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateConnectionResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConnectionResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

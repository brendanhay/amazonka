{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection definition in the Data Catalog.
module Network.AWS.Glue.CreateConnection
  ( -- * Creating a request
    CreateConnection (..),
    mkCreateConnection,

    -- ** Request lenses
    ccCatalogId,
    ccConnectionInput,

    -- * Destructuring the response
    CreateConnectionResponse (..),
    mkCreateConnectionResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { catalogId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateConnection' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which to create the connection. If none is provided, the AWS account ID is used by default.
-- * 'connectionInput' - A @ConnectionInput@ object defining the connection to create.
mkCreateConnection ::
  -- | 'connectionInput'
  ConnectionInput ->
  CreateConnection
mkCreateConnection pConnectionInput_ =
  CreateConnection'
    { catalogId = Lude.Nothing,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which to create the connection. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCatalogId :: Lens.Lens' CreateConnection (Lude.Maybe Lude.Text)
ccCatalogId = Lens.lens (catalogId :: CreateConnection -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: CreateConnection)
{-# DEPRECATED ccCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A @ConnectionInput@ object defining the connection to create.
--
-- /Note:/ Consider using 'connectionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionInput :: Lens.Lens' CreateConnection ConnectionInput
ccConnectionInput = Lens.lens (connectionInput :: CreateConnection -> ConnectionInput) (\s a -> s {connectionInput = a} :: CreateConnection)
{-# DEPRECATED ccConnectionInput "Use generic-lens or generic-optics with 'connectionInput' instead." #-}

instance Lude.AWSRequest CreateConnection where
  type Rs CreateConnection = CreateConnectionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateConnectionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CatalogId" Lude..=) Lude.<$> catalogId,
            Lude.Just ("ConnectionInput" Lude..= connectionInput)
          ]
      )

instance Lude.ToPath CreateConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConnectionResponse' smart constructor.
newtype CreateConnectionResponse = CreateConnectionResponse'
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

-- | Creates a value of 'CreateConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConnectionResponse
mkCreateConnectionResponse pResponseStatus_ =
  CreateConnectionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateConnectionResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConnectionResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

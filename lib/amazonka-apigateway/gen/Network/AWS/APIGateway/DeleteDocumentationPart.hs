{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationPart
  ( -- * Creating a request
    DeleteDocumentationPart (..),
    mkDeleteDocumentationPart,

    -- ** Request lenses
    ddpRestAPIId,
    ddpDocumentationPartId,

    -- * Destructuring the response
    DeleteDocumentationPartResponse (..),
    mkDeleteDocumentationPartResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes an existing documentation part of an API.
--
-- /See:/ 'mkDeleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { restAPIId ::
      Lude.Text,
    documentationPartId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentationPart' with the minimum fields required to make a request.
--
-- * 'documentationPartId' - [Required] The identifier of the to-be-deleted documentation part.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteDocumentationPart ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'documentationPartId'
  Lude.Text ->
  DeleteDocumentationPart
mkDeleteDocumentationPart pRestAPIId_ pDocumentationPartId_ =
  DeleteDocumentationPart'
    { restAPIId = pRestAPIId_,
      documentationPartId = pDocumentationPartId_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpRestAPIId :: Lens.Lens' DeleteDocumentationPart Lude.Text
ddpRestAPIId = Lens.lens (restAPIId :: DeleteDocumentationPart -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteDocumentationPart)
{-# DEPRECATED ddpRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of the to-be-deleted documentation part.
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpDocumentationPartId :: Lens.Lens' DeleteDocumentationPart Lude.Text
ddpDocumentationPartId = Lens.lens (documentationPartId :: DeleteDocumentationPart -> Lude.Text) (\s a -> s {documentationPartId = a} :: DeleteDocumentationPart)
{-# DEPRECATED ddpDocumentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead." #-}

instance Lude.AWSRequest DeleteDocumentationPart where
  type Rs DeleteDocumentationPart = DeleteDocumentationPartResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteDocumentationPartResponse'

instance Lude.ToHeaders DeleteDocumentationPart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteDocumentationPart where
  toPath DeleteDocumentationPart' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/parts/",
        Lude.toBS documentationPartId
      ]

instance Lude.ToQuery DeleteDocumentationPart where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse = DeleteDocumentationPartResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentationPartResponse' with the minimum fields required to make a request.
mkDeleteDocumentationPartResponse ::
  DeleteDocumentationPartResponse
mkDeleteDocumentationPartResponse =
  DeleteDocumentationPartResponse'

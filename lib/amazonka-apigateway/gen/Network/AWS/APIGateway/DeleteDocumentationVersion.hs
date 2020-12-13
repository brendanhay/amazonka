{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationVersion
  ( -- * Creating a request
    DeleteDocumentationVersion (..),
    mkDeleteDocumentationVersion,

    -- ** Request lenses
    ddvDocumentationVersion,
    ddvRestAPIId,

    -- * Destructuring the response
    DeleteDocumentationVersionResponse (..),
    mkDeleteDocumentationVersionResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes an existing documentation version of an API.
--
-- /See:/ 'mkDeleteDocumentationVersion' smart constructor.
data DeleteDocumentationVersion = DeleteDocumentationVersion'
  { -- | [Required] The version identifier of a to-be-deleted documentation snapshot.
    documentationVersion :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentationVersion' with the minimum fields required to make a request.
--
-- * 'documentationVersion' - [Required] The version identifier of a to-be-deleted documentation snapshot.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteDocumentationVersion ::
  -- | 'documentationVersion'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteDocumentationVersion
mkDeleteDocumentationVersion pDocumentationVersion_ pRestAPIId_ =
  DeleteDocumentationVersion'
    { documentationVersion =
        pDocumentationVersion_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The version identifier of a to-be-deleted documentation snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDocumentationVersion :: Lens.Lens' DeleteDocumentationVersion Lude.Text
ddvDocumentationVersion = Lens.lens (documentationVersion :: DeleteDocumentationVersion -> Lude.Text) (\s a -> s {documentationVersion = a} :: DeleteDocumentationVersion)
{-# DEPRECATED ddvDocumentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvRestAPIId :: Lens.Lens' DeleteDocumentationVersion Lude.Text
ddvRestAPIId = Lens.lens (restAPIId :: DeleteDocumentationVersion -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteDocumentationVersion)
{-# DEPRECATED ddvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteDocumentationVersion where
  type
    Rs DeleteDocumentationVersion =
      DeleteDocumentationVersionResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteDocumentationVersionResponse'

instance Lude.ToHeaders DeleteDocumentationVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteDocumentationVersion where
  toPath DeleteDocumentationVersion' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/versions/",
        Lude.toBS documentationVersion
      ]

instance Lude.ToQuery DeleteDocumentationVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDocumentationVersionResponse' smart constructor.
data DeleteDocumentationVersionResponse = DeleteDocumentationVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentationVersionResponse' with the minimum fields required to make a request.
mkDeleteDocumentationVersionResponse ::
  DeleteDocumentationVersionResponse
mkDeleteDocumentationVersionResponse =
  DeleteDocumentationVersionResponse'

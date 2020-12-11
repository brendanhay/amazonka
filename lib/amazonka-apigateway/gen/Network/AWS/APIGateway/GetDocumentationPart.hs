{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationPart
  ( -- * Creating a request
    GetDocumentationPart (..),
    mkGetDocumentationPart,

    -- ** Request lenses
    getRestAPIId,
    getDocumentationPartId,

    -- * Destructuring the response
    DocumentationPart (..),
    mkDocumentationPart,

    -- ** Response lenses
    dpLocation,
    dpId,
    dpProperties,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets a specified documentation part of a given API.
--
-- /See:/ 'mkGetDocumentationPart' smart constructor.
data GetDocumentationPart = GetDocumentationPart'
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

-- | Creates a value of 'GetDocumentationPart' with the minimum fields required to make a request.
--
-- * 'documentationPartId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetDocumentationPart ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'documentationPartId'
  Lude.Text ->
  GetDocumentationPart
mkGetDocumentationPart pRestAPIId_ pDocumentationPartId_ =
  GetDocumentationPart'
    { restAPIId = pRestAPIId_,
      documentationPartId = pDocumentationPartId_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getRestAPIId :: Lens.Lens' GetDocumentationPart Lude.Text
getRestAPIId = Lens.lens (restAPIId :: GetDocumentationPart -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDocumentationPart)
{-# DEPRECATED getRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getDocumentationPartId :: Lens.Lens' GetDocumentationPart Lude.Text
getDocumentationPartId = Lens.lens (documentationPartId :: GetDocumentationPart -> Lude.Text) (\s a -> s {documentationPartId = a} :: GetDocumentationPart)
{-# DEPRECATED getDocumentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead." #-}

instance Lude.AWSRequest GetDocumentationPart where
  type Rs GetDocumentationPart = DocumentationPart
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetDocumentationPart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDocumentationPart where
  toPath GetDocumentationPart' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/parts/",
        Lude.toBS documentationPartId
      ]

instance Lude.ToQuery GetDocumentationPart where
  toQuery = Lude.const Lude.mempty

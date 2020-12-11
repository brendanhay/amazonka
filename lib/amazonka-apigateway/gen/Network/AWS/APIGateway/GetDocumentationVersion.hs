{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationVersion
  ( -- * Creating a request
    GetDocumentationVersion (..),
    mkGetDocumentationVersion,

    -- ** Request lenses
    gdvdRestAPIId,
    gdvdDocumentationVersion,

    -- * Destructuring the response
    DocumentationVersion (..),
    mkDocumentationVersion,

    -- ** Response lenses
    dvCreatedDate,
    dvVersion,
    dvDescription,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets a documentation snapshot of an API.
--
-- /See:/ 'mkGetDocumentationVersion' smart constructor.
data GetDocumentationVersion = GetDocumentationVersion'
  { restAPIId ::
      Lude.Text,
    documentationVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentationVersion' with the minimum fields required to make a request.
--
-- * 'documentationVersion' - [Required] The version identifier of the to-be-retrieved documentation snapshot.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetDocumentationVersion ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'documentationVersion'
  Lude.Text ->
  GetDocumentationVersion
mkGetDocumentationVersion pRestAPIId_ pDocumentationVersion_ =
  GetDocumentationVersion'
    { restAPIId = pRestAPIId_,
      documentationVersion = pDocumentationVersion_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvdRestAPIId :: Lens.Lens' GetDocumentationVersion Lude.Text
gdvdRestAPIId = Lens.lens (restAPIId :: GetDocumentationVersion -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDocumentationVersion)
{-# DEPRECATED gdvdRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The version identifier of the to-be-retrieved documentation snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvdDocumentationVersion :: Lens.Lens' GetDocumentationVersion Lude.Text
gdvdDocumentationVersion = Lens.lens (documentationVersion :: GetDocumentationVersion -> Lude.Text) (\s a -> s {documentationVersion = a} :: GetDocumentationVersion)
{-# DEPRECATED gdvdDocumentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead." #-}

instance Lude.AWSRequest GetDocumentationVersion where
  type Rs GetDocumentationVersion = DocumentationVersion
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetDocumentationVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDocumentationVersion where
  toPath GetDocumentationVersion' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/versions/",
        Lude.toBS documentationVersion
      ]

instance Lude.ToQuery GetDocumentationVersion where
  toQuery = Lude.const Lude.mempty

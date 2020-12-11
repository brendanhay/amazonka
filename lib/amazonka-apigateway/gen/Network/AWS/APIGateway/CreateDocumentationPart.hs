{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationPart
  ( -- * Creating a request
    CreateDocumentationPart (..),
    mkCreateDocumentationPart,

    -- ** Request lenses
    cdpRestAPIId,
    cdpLocation,
    cdpProperties,

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

-- | Creates a new documentation part of a given API.
--
-- /See:/ 'mkCreateDocumentationPart' smart constructor.
data CreateDocumentationPart = CreateDocumentationPart'
  { restAPIId ::
      Lude.Text,
    location :: DocumentationPartLocation,
    properties :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocumentationPart' with the minimum fields required to make a request.
--
-- * 'location' - [Required] The location of the targeted API entity of the to-be-created documentation part.
-- * 'properties' - [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only OpenAPI-compliant key-value pairs can be exported and, hence, published.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkCreateDocumentationPart ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'location'
  DocumentationPartLocation ->
  -- | 'properties'
  Lude.Text ->
  CreateDocumentationPart
mkCreateDocumentationPart pRestAPIId_ pLocation_ pProperties_ =
  CreateDocumentationPart'
    { restAPIId = pRestAPIId_,
      location = pLocation_,
      properties = pProperties_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRestAPIId :: Lens.Lens' CreateDocumentationPart Lude.Text
cdpRestAPIId = Lens.lens (restAPIId :: CreateDocumentationPart -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateDocumentationPart)
{-# DEPRECATED cdpRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The location of the targeted API entity of the to-be-created documentation part.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLocation :: Lens.Lens' CreateDocumentationPart DocumentationPartLocation
cdpLocation = Lens.lens (location :: CreateDocumentationPart -> DocumentationPartLocation) (\s a -> s {location = a} :: CreateDocumentationPart)
{-# DEPRECATED cdpLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only OpenAPI-compliant key-value pairs can be exported and, hence, published.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpProperties :: Lens.Lens' CreateDocumentationPart Lude.Text
cdpProperties = Lens.lens (properties :: CreateDocumentationPart -> Lude.Text) (\s a -> s {properties = a} :: CreateDocumentationPart)
{-# DEPRECATED cdpProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.AWSRequest CreateDocumentationPart where
  type Rs CreateDocumentationPart = DocumentationPart
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateDocumentationPart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateDocumentationPart where
  toJSON CreateDocumentationPart' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("location" Lude..= location),
            Lude.Just ("properties" Lude..= properties)
          ]
      )

instance Lude.ToPath CreateDocumentationPart where
  toPath CreateDocumentationPart' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/documentation/parts"]

instance Lude.ToQuery CreateDocumentationPart where
  toQuery = Lude.const Lude.mempty

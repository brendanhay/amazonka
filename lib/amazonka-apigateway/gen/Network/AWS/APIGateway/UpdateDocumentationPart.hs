{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.UpdateDocumentationPart
  ( -- * Creating a request
    UpdateDocumentationPart (..),
    mkUpdateDocumentationPart,

    -- ** Request lenses
    udpDocumentationPartId,
    udpRestAPIId,
    udpPatchOperations,

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

-- | Updates an existing documentation part of a given API.
--
-- /See:/ 'mkUpdateDocumentationPart' smart constructor.
data UpdateDocumentationPart = UpdateDocumentationPart'
  { -- | [Required] The identifier of the to-be-updated documentation part.
    documentationPartId :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentationPart' with the minimum fields required to make a request.
--
-- * 'documentationPartId' - [Required] The identifier of the to-be-updated documentation part.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateDocumentationPart ::
  -- | 'documentationPartId'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  UpdateDocumentationPart
mkUpdateDocumentationPart pDocumentationPartId_ pRestAPIId_ =
  UpdateDocumentationPart'
    { documentationPartId =
        pDocumentationPartId_,
      restAPIId = pRestAPIId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The identifier of the to-be-updated documentation part.
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpDocumentationPartId :: Lens.Lens' UpdateDocumentationPart Lude.Text
udpDocumentationPartId = Lens.lens (documentationPartId :: UpdateDocumentationPart -> Lude.Text) (\s a -> s {documentationPartId = a} :: UpdateDocumentationPart)
{-# DEPRECATED udpDocumentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpRestAPIId :: Lens.Lens' UpdateDocumentationPart Lude.Text
udpRestAPIId = Lens.lens (restAPIId :: UpdateDocumentationPart -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateDocumentationPart)
{-# DEPRECATED udpRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpPatchOperations :: Lens.Lens' UpdateDocumentationPart (Lude.Maybe [PatchOperation])
udpPatchOperations = Lens.lens (patchOperations :: UpdateDocumentationPart -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateDocumentationPart)
{-# DEPRECATED udpPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateDocumentationPart where
  type Rs UpdateDocumentationPart = DocumentationPart
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateDocumentationPart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateDocumentationPart where
  toJSON UpdateDocumentationPart' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateDocumentationPart where
  toPath UpdateDocumentationPart' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/parts/",
        Lude.toBS documentationPartId
      ]

instance Lude.ToQuery UpdateDocumentationPart where
  toQuery = Lude.const Lude.mempty

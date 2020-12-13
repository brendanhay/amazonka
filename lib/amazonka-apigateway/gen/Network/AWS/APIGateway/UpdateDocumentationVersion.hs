{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.UpdateDocumentationVersion
  ( -- * Creating a request
    UpdateDocumentationVersion (..),
    mkUpdateDocumentationVersion,

    -- ** Request lenses
    udvDocumentationVersion,
    udvRestAPIId,
    udvPatchOperations,

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

-- | Updates an existing documentation version of an API.
--
-- /See:/ 'mkUpdateDocumentationVersion' smart constructor.
data UpdateDocumentationVersion = UpdateDocumentationVersion'
  { -- | [Required] The version identifier of the to-be-updated documentation version.
    documentationVersion :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' ..
    restAPIId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentationVersion' with the minimum fields required to make a request.
--
-- * 'documentationVersion' - [Required] The version identifier of the to-be-updated documentation version.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' ..
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateDocumentationVersion ::
  -- | 'documentationVersion'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  UpdateDocumentationVersion
mkUpdateDocumentationVersion pDocumentationVersion_ pRestAPIId_ =
  UpdateDocumentationVersion'
    { documentationVersion =
        pDocumentationVersion_,
      restAPIId = pRestAPIId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The version identifier of the to-be-updated documentation version.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvDocumentationVersion :: Lens.Lens' UpdateDocumentationVersion Lude.Text
udvDocumentationVersion = Lens.lens (documentationVersion :: UpdateDocumentationVersion -> Lude.Text) (\s a -> s {documentationVersion = a} :: UpdateDocumentationVersion)
{-# DEPRECATED udvDocumentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' ..
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvRestAPIId :: Lens.Lens' UpdateDocumentationVersion Lude.Text
udvRestAPIId = Lens.lens (restAPIId :: UpdateDocumentationVersion -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateDocumentationVersion)
{-# DEPRECATED udvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvPatchOperations :: Lens.Lens' UpdateDocumentationVersion (Lude.Maybe [PatchOperation])
udvPatchOperations = Lens.lens (patchOperations :: UpdateDocumentationVersion -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateDocumentationVersion)
{-# DEPRECATED udvPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateDocumentationVersion where
  type Rs UpdateDocumentationVersion = DocumentationVersion
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateDocumentationVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateDocumentationVersion where
  toJSON UpdateDocumentationVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateDocumentationVersion where
  toPath UpdateDocumentationVersion' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/documentation/versions/",
        Lude.toBS documentationVersion
      ]

instance Lude.ToQuery UpdateDocumentationVersion where
  toQuery = Lude.const Lude.mempty

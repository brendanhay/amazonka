{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationVersion
  ( -- * Creating a request
    CreateDocumentationVersion (..),
    mkCreateDocumentationVersion,

    -- ** Request lenses
    cdvDocumentationVersion,
    cdvRestAPIId,
    cdvStageName,
    cdvDescription,

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

-- | Creates a new documentation version of a given API.
--
-- /See:/ 'mkCreateDocumentationVersion' smart constructor.
data CreateDocumentationVersion = CreateDocumentationVersion'
  { -- | [Required] The version identifier of the new snapshot.
    documentationVersion :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The stage name to be associated with the new documentation snapshot.
    stageName :: Lude.Maybe Lude.Text,
    -- | A description about the new documentation snapshot.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDocumentationVersion' with the minimum fields required to make a request.
--
-- * 'documentationVersion' - [Required] The version identifier of the new snapshot.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageName' - The stage name to be associated with the new documentation snapshot.
-- * 'description' - A description about the new documentation snapshot.
mkCreateDocumentationVersion ::
  -- | 'documentationVersion'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  CreateDocumentationVersion
mkCreateDocumentationVersion pDocumentationVersion_ pRestAPIId_ =
  CreateDocumentationVersion'
    { documentationVersion =
        pDocumentationVersion_,
      restAPIId = pRestAPIId_,
      stageName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | [Required] The version identifier of the new snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDocumentationVersion :: Lens.Lens' CreateDocumentationVersion Lude.Text
cdvDocumentationVersion = Lens.lens (documentationVersion :: CreateDocumentationVersion -> Lude.Text) (\s a -> s {documentationVersion = a} :: CreateDocumentationVersion)
{-# DEPRECATED cdvDocumentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvRestAPIId :: Lens.Lens' CreateDocumentationVersion Lude.Text
cdvRestAPIId = Lens.lens (restAPIId :: CreateDocumentationVersion -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateDocumentationVersion)
{-# DEPRECATED cdvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The stage name to be associated with the new documentation snapshot.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvStageName :: Lens.Lens' CreateDocumentationVersion (Lude.Maybe Lude.Text)
cdvStageName = Lens.lens (stageName :: CreateDocumentationVersion -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: CreateDocumentationVersion)
{-# DEPRECATED cdvStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | A description about the new documentation snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDescription :: Lens.Lens' CreateDocumentationVersion (Lude.Maybe Lude.Text)
cdvDescription = Lens.lens (description :: CreateDocumentationVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDocumentationVersion)
{-# DEPRECATED cdvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateDocumentationVersion where
  type Rs CreateDocumentationVersion = DocumentationVersion
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateDocumentationVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateDocumentationVersion where
  toJSON CreateDocumentationVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("documentationVersion" Lude..= documentationVersion),
            ("stageName" Lude..=) Lude.<$> stageName,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateDocumentationVersion where
  toPath CreateDocumentationVersion' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/documentation/versions"]

instance Lude.ToQuery CreateDocumentationVersion where
  toQuery = Lude.const Lude.mempty

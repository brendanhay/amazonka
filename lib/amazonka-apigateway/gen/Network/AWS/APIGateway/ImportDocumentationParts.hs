{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.ImportDocumentationParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.ImportDocumentationParts
  ( -- * Creating a request
    ImportDocumentationParts (..),
    mkImportDocumentationParts,

    -- ** Request lenses
    idpMode,
    idpFailOnWarnings,
    idpRestAPIId,
    idpBody,

    -- * Destructuring the response
    ImportDocumentationPartsResponse (..),
    mkImportDocumentationPartsResponse,

    -- ** Response lenses
    idprsIds,
    idprsWarnings,
    idprsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Import documentation parts from an external (e.g., OpenAPI) definition file.
--
-- /See:/ 'mkImportDocumentationParts' smart constructor.
data ImportDocumentationParts = ImportDocumentationParts'
  { mode ::
      Lude.Maybe PutMode,
    failOnWarnings :: Lude.Maybe Lude.Bool,
    restAPIId :: Lude.Text,
    body :: Lude.ByteString
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportDocumentationParts' with the minimum fields required to make a request.
--
-- * 'body' - [Required] Raw byte array representing the to-be-imported documentation parts. To import from an OpenAPI file, this is a JSON object.
-- * 'failOnWarnings' - A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
-- * 'mode' - A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkImportDocumentationParts ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'body'
  Lude.ByteString ->
  ImportDocumentationParts
mkImportDocumentationParts pRestAPIId_ pBody_ =
  ImportDocumentationParts'
    { mode = Lude.Nothing,
      failOnWarnings = Lude.Nothing,
      restAPIId = pRestAPIId_,
      body = pBody_
    }

-- | A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpMode :: Lens.Lens' ImportDocumentationParts (Lude.Maybe PutMode)
idpMode = Lens.lens (mode :: ImportDocumentationParts -> Lude.Maybe PutMode) (\s a -> s {mode = a} :: ImportDocumentationParts)
{-# DEPRECATED idpMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpFailOnWarnings :: Lens.Lens' ImportDocumentationParts (Lude.Maybe Lude.Bool)
idpFailOnWarnings = Lens.lens (failOnWarnings :: ImportDocumentationParts -> Lude.Maybe Lude.Bool) (\s a -> s {failOnWarnings = a} :: ImportDocumentationParts)
{-# DEPRECATED idpFailOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpRestAPIId :: Lens.Lens' ImportDocumentationParts Lude.Text
idpRestAPIId = Lens.lens (restAPIId :: ImportDocumentationParts -> Lude.Text) (\s a -> s {restAPIId = a} :: ImportDocumentationParts)
{-# DEPRECATED idpRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Raw byte array representing the to-be-imported documentation parts. To import from an OpenAPI file, this is a JSON object.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpBody :: Lens.Lens' ImportDocumentationParts Lude.ByteString
idpBody = Lens.lens (body :: ImportDocumentationParts -> Lude.ByteString) (\s a -> s {body = a} :: ImportDocumentationParts)
{-# DEPRECATED idpBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.AWSRequest ImportDocumentationParts where
  type Rs ImportDocumentationParts = ImportDocumentationPartsResponse
  request = Req.putBody apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportDocumentationPartsResponse'
            Lude.<$> (x Lude..?> "ids" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "warnings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody ImportDocumentationParts where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders ImportDocumentationParts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath ImportDocumentationParts where
  toPath ImportDocumentationParts' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/documentation/parts"]

instance Lude.ToQuery ImportDocumentationParts where
  toQuery ImportDocumentationParts' {..} =
    Lude.mconcat
      ["mode" Lude.=: mode, "failonwarnings" Lude.=: failOnWarnings]

-- | A collection of the imported 'DocumentationPart' identifiers.
--
-- This is used to return the result when documentation parts in an external (e.g., OpenAPI) file are imported into API Gateway<https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , <https://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/ documentationpart:import> , 'DocumentationPart'
--
-- /See:/ 'mkImportDocumentationPartsResponse' smart constructor.
data ImportDocumentationPartsResponse = ImportDocumentationPartsResponse'
  { ids ::
      Lude.Maybe [Lude.Text],
    warnings ::
      Lude.Maybe [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportDocumentationPartsResponse' with the minimum fields required to make a request.
--
-- * 'ids' - A list of the returned documentation part identifiers.
-- * 'responseStatus' - The response status code.
-- * 'warnings' - A list of warning messages reported during import of documentation parts.
mkImportDocumentationPartsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportDocumentationPartsResponse
mkImportDocumentationPartsResponse pResponseStatus_ =
  ImportDocumentationPartsResponse'
    { ids = Lude.Nothing,
      warnings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the returned documentation part identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprsIds :: Lens.Lens' ImportDocumentationPartsResponse (Lude.Maybe [Lude.Text])
idprsIds = Lens.lens (ids :: ImportDocumentationPartsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ids = a} :: ImportDocumentationPartsResponse)
{-# DEPRECATED idprsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | A list of warning messages reported during import of documentation parts.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprsWarnings :: Lens.Lens' ImportDocumentationPartsResponse (Lude.Maybe [Lude.Text])
idprsWarnings = Lens.lens (warnings :: ImportDocumentationPartsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {warnings = a} :: ImportDocumentationPartsResponse)
{-# DEPRECATED idprsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprsResponseStatus :: Lens.Lens' ImportDocumentationPartsResponse Lude.Int
idprsResponseStatus = Lens.lens (responseStatus :: ImportDocumentationPartsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportDocumentationPartsResponse)
{-# DEPRECATED idprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

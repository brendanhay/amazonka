{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetSchemaAsJSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a JSON representation of the schema. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format> for more information.
module Network.AWS.CloudDirectory.GetSchemaAsJSON
  ( -- * Creating a request
    GetSchemaAsJSON (..),
    mkGetSchemaAsJSON,

    -- ** Request lenses
    gsajSchemaARN,

    -- * Destructuring the response
    GetSchemaAsJSONResponse (..),
    mkGetSchemaAsJSONResponse,

    -- ** Response lenses
    gsajrsDocument,
    gsajrsName,
    gsajrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchemaAsJSON' smart constructor.
newtype GetSchemaAsJSON = GetSchemaAsJSON'
  { -- | The ARN of the schema to retrieve.
    schemaARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaAsJSON' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The ARN of the schema to retrieve.
mkGetSchemaAsJSON ::
  -- | 'schemaARN'
  Lude.Text ->
  GetSchemaAsJSON
mkGetSchemaAsJSON pSchemaARN_ =
  GetSchemaAsJSON' {schemaARN = pSchemaARN_}

-- | The ARN of the schema to retrieve.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajSchemaARN :: Lens.Lens' GetSchemaAsJSON Lude.Text
gsajSchemaARN = Lens.lens (schemaARN :: GetSchemaAsJSON -> Lude.Text) (\s a -> s {schemaARN = a} :: GetSchemaAsJSON)
{-# DEPRECATED gsajSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.AWSRequest GetSchemaAsJSON where
  type Rs GetSchemaAsJSON = GetSchemaAsJSONResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaAsJSONResponse'
            Lude.<$> (x Lude..?> "Document")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchemaAsJSON where
  toHeaders GetSchemaAsJSON' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON GetSchemaAsJSON where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetSchemaAsJSON where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/schema/json"

instance Lude.ToQuery GetSchemaAsJSON where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaAsJSONResponse' smart constructor.
data GetSchemaAsJSONResponse = GetSchemaAsJSONResponse'
  { -- | The JSON representation of the schema document.
    document :: Lude.Maybe Lude.Text,
    -- | The name of the retrieved schema.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaAsJSONResponse' with the minimum fields required to make a request.
--
-- * 'document' - The JSON representation of the schema document.
-- * 'name' - The name of the retrieved schema.
-- * 'responseStatus' - The response status code.
mkGetSchemaAsJSONResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaAsJSONResponse
mkGetSchemaAsJSONResponse pResponseStatus_ =
  GetSchemaAsJSONResponse'
    { document = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The JSON representation of the schema document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrsDocument :: Lens.Lens' GetSchemaAsJSONResponse (Lude.Maybe Lude.Text)
gsajrsDocument = Lens.lens (document :: GetSchemaAsJSONResponse -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: GetSchemaAsJSONResponse)
{-# DEPRECATED gsajrsDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The name of the retrieved schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrsName :: Lens.Lens' GetSchemaAsJSONResponse (Lude.Maybe Lude.Text)
gsajrsName = Lens.lens (name :: GetSchemaAsJSONResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetSchemaAsJSONResponse)
{-# DEPRECATED gsajrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsajrsResponseStatus :: Lens.Lens' GetSchemaAsJSONResponse Lude.Int
gsajrsResponseStatus = Lens.lens (responseStatus :: GetSchemaAsJSONResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaAsJSONResponse)
{-# DEPRECATED gsajrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

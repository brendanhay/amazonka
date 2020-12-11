{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.PutSchemaFromJSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a schema to be updated using JSON upload. Only available for development schemas. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format> for more information.
module Network.AWS.CloudDirectory.PutSchemaFromJSON
  ( -- * Creating a request
    PutSchemaFromJSON (..),
    mkPutSchemaFromJSON,

    -- ** Request lenses
    psfjSchemaARN,
    psfjDocument,

    -- * Destructuring the response
    PutSchemaFromJSONResponse (..),
    mkPutSchemaFromJSONResponse,

    -- ** Response lenses
    psfjrsARN,
    psfjrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSchemaFromJSON' smart constructor.
data PutSchemaFromJSON = PutSchemaFromJSON'
  { schemaARN :: Lude.Text,
    document :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSchemaFromJSON' with the minimum fields required to make a request.
--
-- * 'document' - The replacement JSON schema.
-- * 'schemaARN' - The ARN of the schema to update.
mkPutSchemaFromJSON ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'document'
  Lude.Text ->
  PutSchemaFromJSON
mkPutSchemaFromJSON pSchemaARN_ pDocument_ =
  PutSchemaFromJSON'
    { schemaARN = pSchemaARN_,
      document = pDocument_
    }

-- | The ARN of the schema to update.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjSchemaARN :: Lens.Lens' PutSchemaFromJSON Lude.Text
psfjSchemaARN = Lens.lens (schemaARN :: PutSchemaFromJSON -> Lude.Text) (\s a -> s {schemaARN = a} :: PutSchemaFromJSON)
{-# DEPRECATED psfjSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The replacement JSON schema.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjDocument :: Lens.Lens' PutSchemaFromJSON Lude.Text
psfjDocument = Lens.lens (document :: PutSchemaFromJSON -> Lude.Text) (\s a -> s {document = a} :: PutSchemaFromJSON)
{-# DEPRECATED psfjDocument "Use generic-lens or generic-optics with 'document' instead." #-}

instance Lude.AWSRequest PutSchemaFromJSON where
  type Rs PutSchemaFromJSON = PutSchemaFromJSONResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutSchemaFromJSONResponse'
            Lude.<$> (x Lude..?> "Arn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutSchemaFromJSON where
  toHeaders PutSchemaFromJSON' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON PutSchemaFromJSON where
  toJSON PutSchemaFromJSON' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Document" Lude..= document)])

instance Lude.ToPath PutSchemaFromJSON where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/schema/json"

instance Lude.ToQuery PutSchemaFromJSON where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSchemaFromJSONResponse' smart constructor.
data PutSchemaFromJSONResponse = PutSchemaFromJSONResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSchemaFromJSONResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the schema to update.
-- * 'responseStatus' - The response status code.
mkPutSchemaFromJSONResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutSchemaFromJSONResponse
mkPutSchemaFromJSONResponse pResponseStatus_ =
  PutSchemaFromJSONResponse'
    { arn = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the schema to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjrsARN :: Lens.Lens' PutSchemaFromJSONResponse (Lude.Maybe Lude.Text)
psfjrsARN = Lens.lens (arn :: PutSchemaFromJSONResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PutSchemaFromJSONResponse)
{-# DEPRECATED psfjrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfjrsResponseStatus :: Lens.Lens' PutSchemaFromJSONResponse Lude.Int
psfjrsResponseStatus = Lens.lens (responseStatus :: PutSchemaFromJSONResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSchemaFromJSONResponse)
{-# DEPRECATED psfjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

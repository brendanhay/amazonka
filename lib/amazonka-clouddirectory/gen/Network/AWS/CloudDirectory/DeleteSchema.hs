{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given schema. Schemas in a development and published state can only be deleted.
module Network.AWS.CloudDirectory.DeleteSchema
  ( -- * Creating a request
    DeleteSchema (..),
    mkDeleteSchema,

    -- ** Request lenses
    dsSchemaARN,

    -- * Destructuring the response
    DeleteSchemaResponse (..),
    mkDeleteSchemaResponse,

    -- ** Response lenses
    dsrsSchemaARN,
    dsrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema' {schemaARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSchema' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
mkDeleteSchema ::
  -- | 'schemaARN'
  Lude.Text ->
  DeleteSchema
mkDeleteSchema pSchemaARN_ = DeleteSchema' {schemaARN = pSchemaARN_}

-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSchemaARN :: Lens.Lens' DeleteSchema Lude.Text
dsSchemaARN = Lens.lens (schemaARN :: DeleteSchema -> Lude.Text) (\s a -> s {schemaARN = a} :: DeleteSchema)
{-# DEPRECATED dsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.AWSRequest DeleteSchema where
  type Rs DeleteSchema = DeleteSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            Lude.<$> (x Lude..?> "SchemaArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSchema where
  toHeaders DeleteSchema' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON DeleteSchema where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteSchema where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/schema"

instance Lude.ToQuery DeleteSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { schemaARN ::
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

-- | Creates a value of 'DeleteSchemaResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'schemaARN' - The input ARN that is returned as part of the response. For more information, see 'arns' .
mkDeleteSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSchemaResponse
mkDeleteSchemaResponse pResponseStatus_ =
  DeleteSchemaResponse'
    { schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The input ARN that is returned as part of the response. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSchemaARN :: Lens.Lens' DeleteSchemaResponse (Lude.Maybe Lude.Text)
dsrsSchemaARN = Lens.lens (schemaARN :: DeleteSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: DeleteSchemaResponse)
{-# DEPRECATED dsrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSchemaResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSchemaResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

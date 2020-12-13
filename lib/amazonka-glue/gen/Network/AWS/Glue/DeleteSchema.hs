{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the entire schema set, including the schema set and all of its versions. To get the status of the delete operation, you can call @GetSchema@ API after the asynchronous call. Deleting a registry will disable all online operations for the schema, such as the @GetSchemaByDefinition@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteSchema
  ( -- * Creating a request
    DeleteSchema (..),
    mkDeleteSchema,

    -- ** Request lenses
    dsSchemaId,

    -- * Destructuring the response
    DeleteSchemaResponse (..),
    mkDeleteSchemaResponse,

    -- ** Response lenses
    dsrsStatus,
    dsrsSchemaName,
    dsrsSchemaARN,
    dsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema'
  { -- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
    schemaId :: SchemaId
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSchema' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
mkDeleteSchema ::
  -- | 'schemaId'
  SchemaId ->
  DeleteSchema
mkDeleteSchema pSchemaId_ = DeleteSchema' {schemaId = pSchemaId_}

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSchemaId :: Lens.Lens' DeleteSchema SchemaId
dsSchemaId = Lens.lens (schemaId :: DeleteSchema -> SchemaId) (\s a -> s {schemaId = a} :: DeleteSchema)
{-# DEPRECATED dsSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

instance Lude.AWSRequest DeleteSchema where
  type Rs DeleteSchema = DeleteSchemaResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "SchemaName")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteSchema" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSchema where
  toJSON DeleteSchema' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SchemaId" Lude..= schemaId)])

instance Lude.ToPath DeleteSchema where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { -- | The status of the schema.
    status :: Lude.Maybe SchemaStatus,
    -- | The name of the schema being deleted.
    schemaName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the schema being deleted.
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSchemaResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the schema.
-- * 'schemaName' - The name of the schema being deleted.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSchemaResponse
mkDeleteSchemaResponse pResponseStatus_ =
  DeleteSchemaResponse'
    { status = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the schema.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsStatus :: Lens.Lens' DeleteSchemaResponse (Lude.Maybe SchemaStatus)
dsrsStatus = Lens.lens (status :: DeleteSchemaResponse -> Lude.Maybe SchemaStatus) (\s a -> s {status = a} :: DeleteSchemaResponse)
{-# DEPRECATED dsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the schema being deleted.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSchemaName :: Lens.Lens' DeleteSchemaResponse (Lude.Maybe Lude.Text)
dsrsSchemaName = Lens.lens (schemaName :: DeleteSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: DeleteSchemaResponse)
{-# DEPRECATED dsrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema being deleted.
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

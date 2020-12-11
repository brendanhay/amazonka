{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema name with a new name. Only development schema names can be updated.
module Network.AWS.CloudDirectory.UpdateSchema
  ( -- * Creating a request
    UpdateSchema (..),
    mkUpdateSchema,

    -- ** Request lenses
    usSchemaARN,
    usName,

    -- * Destructuring the response
    UpdateSchemaResponse (..),
    mkUpdateSchemaResponse,

    -- ** Response lenses
    usrsSchemaARN,
    usrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { schemaARN :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSchema' with the minimum fields required to make a request.
--
-- * 'name' - The name of the schema.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
mkUpdateSchema ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateSchema
mkUpdateSchema pSchemaARN_ pName_ =
  UpdateSchema' {schemaARN = pSchemaARN_, name = pName_}

-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaARN :: Lens.Lens' UpdateSchema Lude.Text
usSchemaARN = Lens.lens (schemaARN :: UpdateSchema -> Lude.Text) (\s a -> s {schemaARN = a} :: UpdateSchema)
{-# DEPRECATED usSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateSchema Lude.Text
usName = Lens.lens (name :: UpdateSchema -> Lude.Text) (\s a -> s {name = a} :: UpdateSchema)
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Lude.<$> (x Lude..?> "SchemaArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSchema where
  toHeaders UpdateSchema' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath UpdateSchema where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/update"

instance Lude.ToQuery UpdateSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
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

-- | Creates a value of 'UpdateSchemaResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'schemaARN' - The ARN that is associated with the updated schema. For more information, see 'arns' .
mkUpdateSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSchemaResponse
mkUpdateSchemaResponse pResponseStatus_ =
  UpdateSchemaResponse'
    { schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN that is associated with the updated schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsSchemaARN :: Lens.Lens' UpdateSchemaResponse (Lude.Maybe Lude.Text)
usrsSchemaARN = Lens.lens (schemaARN :: UpdateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateSchemaResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

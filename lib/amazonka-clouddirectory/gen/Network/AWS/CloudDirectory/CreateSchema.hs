{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema in a development state. A schema can exist in three phases:
--
--
--     * /Development:/ This is a mutable phase of the schema. All new schemas are in the development phase. Once the schema is finalized, it can be published.
--
--
--     * /Published:/ Published schemas are immutable and have a version associated with them.
--
--
--     * /Applied:/ Applied schemas are mutable in a way that allows you to add new schema facets. You can also add new, nonrequired attributes to existing schema facets. You can apply only published schemas to directories.
module Network.AWS.CloudDirectory.CreateSchema
  ( -- * Creating a request
    CreateSchema (..),
    mkCreateSchema,

    -- ** Request lenses
    csName,

    -- * Destructuring the response
    CreateSchemaResponse (..),
    mkCreateSchemaResponse,

    -- ** Response lenses
    csrsSchemaARN,
    csrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSchema' smart constructor.
newtype CreateSchema = CreateSchema'
  { -- | The name that is associated with the schema. This is unique to each account and in each region.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSchema' with the minimum fields required to make a request.
--
-- * 'name' - The name that is associated with the schema. This is unique to each account and in each region.
mkCreateSchema ::
  -- | 'name'
  Lude.Text ->
  CreateSchema
mkCreateSchema pName_ = CreateSchema' {name = pName_}

-- | The name that is associated with the schema. This is unique to each account and in each region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSchema Lude.Text
csName = Lens.lens (name :: CreateSchema -> Lude.Text) (\s a -> s {name = a} :: CreateSchema)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Lude.<$> (x Lude..?> "SchemaArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSchema where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath CreateSchema where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/create"

instance Lude.ToQuery CreateSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSchemaResponse' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
-- * 'responseStatus' - The response status code.
mkCreateSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSchemaResponse
mkCreateSchemaResponse pResponseStatus_ =
  CreateSchemaResponse'
    { schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaARN :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsSchemaARN = Lens.lens (schemaARN :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSchemaResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

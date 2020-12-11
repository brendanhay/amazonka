{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Type@ object.
module Network.AWS.AppSync.CreateType
  ( -- * Creating a request
    CreateType (..),
    mkCreateType,

    -- ** Request lenses
    ctApiId,
    ctDefinition,
    ctFormat,

    -- * Destructuring the response
    CreateTypeResponse (..),
    mkCreateTypeResponse,

    -- ** Response lenses
    ctrsType,
    ctrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateType' smart constructor.
data CreateType = CreateType'
  { apiId :: Lude.Text,
    definition :: Lude.Text,
    format :: TypeDefinitionFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateType' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'definition' - The type definition, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
-- * 'format' - The type format: SDL or JSON.
mkCreateType ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'definition'
  Lude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  CreateType
mkCreateType pApiId_ pDefinition_ pFormat_ =
  CreateType'
    { apiId = pApiId_,
      definition = pDefinition_,
      format = pFormat_
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctApiId :: Lens.Lens' CreateType Lude.Text
ctApiId = Lens.lens (apiId :: CreateType -> Lude.Text) (\s a -> s {apiId = a} :: CreateType)
{-# DEPRECATED ctApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type definition, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDefinition :: Lens.Lens' CreateType Lude.Text
ctDefinition = Lens.lens (definition :: CreateType -> Lude.Text) (\s a -> s {definition = a} :: CreateType)
{-# DEPRECATED ctDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctFormat :: Lens.Lens' CreateType TypeDefinitionFormat
ctFormat = Lens.lens (format :: CreateType -> TypeDefinitionFormat) (\s a -> s {format = a} :: CreateType)
{-# DEPRECATED ctFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.AWSRequest CreateType where
  type Rs CreateType = CreateTypeResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTypeResponse'
            Lude.<$> (x Lude..?> "type") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateType where
  toJSON CreateType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("definition" Lude..= definition),
            Lude.Just ("format" Lude..= format)
          ]
      )

instance Lude.ToPath CreateType where
  toPath CreateType' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/types"]

instance Lude.ToQuery CreateType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTypeResponse' smart constructor.
data CreateTypeResponse = CreateTypeResponse'
  { type' ::
      Lude.Maybe Type,
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

-- | Creates a value of 'CreateTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'type'' - The @Type@ object.
mkCreateTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTypeResponse
mkCreateTypeResponse pResponseStatus_ =
  CreateTypeResponse'
    { type' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsType :: Lens.Lens' CreateTypeResponse (Lude.Maybe Type)
ctrsType = Lens.lens (type' :: CreateTypeResponse -> Lude.Maybe Type) (\s a -> s {type' = a} :: CreateTypeResponse)
{-# DEPRECATED ctrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTypeResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTypeResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

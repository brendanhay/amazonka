{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Type@ object.
module Network.AWS.AppSync.UpdateType
  ( -- * Creating a request
    UpdateType (..),
    mkUpdateType,

    -- ** Request lenses
    utDefinition,
    utApiId,
    utTypeName,
    utFormat,

    -- * Destructuring the response
    UpdateTypeResponse (..),
    mkUpdateTypeResponse,

    -- ** Response lenses
    utrsType,
    utrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateType' smart constructor.
data UpdateType = UpdateType'
  { definition :: Lude.Maybe Lude.Text,
    apiId :: Lude.Text,
    typeName :: Lude.Text,
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

-- | Creates a value of 'UpdateType' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'definition' - The new definition.
-- * 'format' - The new type format: SDL or JSON.
-- * 'typeName' - The new type name.
mkUpdateType ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  UpdateType
mkUpdateType pApiId_ pTypeName_ pFormat_ =
  UpdateType'
    { definition = Lude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      format = pFormat_
    }

-- | The new definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDefinition :: Lens.Lens' UpdateType (Lude.Maybe Lude.Text)
utDefinition = Lens.lens (definition :: UpdateType -> Lude.Maybe Lude.Text) (\s a -> s {definition = a} :: UpdateType)
{-# DEPRECATED utDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utApiId :: Lens.Lens' UpdateType Lude.Text
utApiId = Lens.lens (apiId :: UpdateType -> Lude.Text) (\s a -> s {apiId = a} :: UpdateType)
{-# DEPRECATED utApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTypeName :: Lens.Lens' UpdateType Lude.Text
utTypeName = Lens.lens (typeName :: UpdateType -> Lude.Text) (\s a -> s {typeName = a} :: UpdateType)
{-# DEPRECATED utTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The new type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utFormat :: Lens.Lens' UpdateType TypeDefinitionFormat
utFormat = Lens.lens (format :: UpdateType -> TypeDefinitionFormat) (\s a -> s {format = a} :: UpdateType)
{-# DEPRECATED utFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.AWSRequest UpdateType where
  type Rs UpdateType = UpdateTypeResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTypeResponse'
            Lude.<$> (x Lude..?> "type") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateType where
  toJSON UpdateType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("definition" Lude..=) Lude.<$> definition,
            Lude.Just ("format" Lude..= format)
          ]
      )

instance Lude.ToPath UpdateType where
  toPath UpdateType' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/types/", Lude.toBS typeName]

instance Lude.ToQuery UpdateType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTypeResponse' smart constructor.
data UpdateTypeResponse = UpdateTypeResponse'
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

-- | Creates a value of 'UpdateTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'type'' - The updated @Type@ object.
mkUpdateTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTypeResponse
mkUpdateTypeResponse pResponseStatus_ =
  UpdateTypeResponse'
    { type' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsType :: Lens.Lens' UpdateTypeResponse (Lude.Maybe Type)
utrsType = Lens.lens (type' :: UpdateTypeResponse -> Lude.Maybe Type) (\s a -> s {type' = a} :: UpdateTypeResponse)
{-# DEPRECATED utrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTypeResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTypeResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

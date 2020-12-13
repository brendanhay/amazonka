{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Type@ object.
module Network.AWS.AppSync.GetType
  ( -- * Creating a request
    GetType (..),
    mkGetType,

    -- ** Request lenses
    gtTypeName,
    gtApiId,
    gtFormat,

    -- * Destructuring the response
    GetTypeResponse (..),
    mkGetTypeResponse,

    -- ** Response lenses
    gtrsType,
    gtrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetType' smart constructor.
data GetType = GetType'
  { -- | The type name.
    typeName :: Lude.Text,
    -- | The API ID.
    apiId :: Lude.Text,
    -- | The type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetType' with the minimum fields required to make a request.
--
-- * 'typeName' - The type name.
-- * 'apiId' - The API ID.
-- * 'format' - The type format: SDL or JSON.
mkGetType ::
  -- | 'typeName'
  Lude.Text ->
  -- | 'apiId'
  Lude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  GetType
mkGetType pTypeName_ pApiId_ pFormat_ =
  GetType'
    { typeName = pTypeName_,
      apiId = pApiId_,
      format = pFormat_
    }

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTypeName :: Lens.Lens' GetType Lude.Text
gtTypeName = Lens.lens (typeName :: GetType -> Lude.Text) (\s a -> s {typeName = a} :: GetType)
{-# DEPRECATED gtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtApiId :: Lens.Lens' GetType Lude.Text
gtApiId = Lens.lens (apiId :: GetType -> Lude.Text) (\s a -> s {apiId = a} :: GetType)
{-# DEPRECATED gtApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtFormat :: Lens.Lens' GetType TypeDefinitionFormat
gtFormat = Lens.lens (format :: GetType -> TypeDefinitionFormat) (\s a -> s {format = a} :: GetType)
{-# DEPRECATED gtFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.AWSRequest GetType where
  type Rs GetType = GetTypeResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTypeResponse'
            Lude.<$> (x Lude..?> "type") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetType where
  toPath GetType' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/types/", Lude.toBS typeName]

instance Lude.ToQuery GetType where
  toQuery GetType' {..} = Lude.mconcat ["format" Lude.=: format]

-- | /See:/ 'mkGetTypeResponse' smart constructor.
data GetTypeResponse = GetTypeResponse'
  { -- | The @Type@ object.
    type' :: Lude.Maybe Type,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTypeResponse' with the minimum fields required to make a request.
--
-- * 'type'' - The @Type@ object.
-- * 'responseStatus' - The response status code.
mkGetTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTypeResponse
mkGetTypeResponse pResponseStatus_ =
  GetTypeResponse'
    { type' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsType :: Lens.Lens' GetTypeResponse (Lude.Maybe Type)
gtrsType = Lens.lens (type' :: GetTypeResponse -> Lude.Maybe Type) (\s a -> s {type' = a} :: GetTypeResponse)
{-# DEPRECATED gtrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTypeResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTypeResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

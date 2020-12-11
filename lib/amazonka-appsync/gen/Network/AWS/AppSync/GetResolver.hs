{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Resolver@ object.
module Network.AWS.AppSync.GetResolver
  ( -- * Creating a request
    GetResolver (..),
    mkGetResolver,

    -- ** Request lenses
    grApiId,
    grTypeName,
    grFieldName,

    -- * Destructuring the response
    GetResolverResponse (..),
    mkGetResolverResponse,

    -- ** Response lenses
    grrsResolver,
    grrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResolver' smart constructor.
data GetResolver = GetResolver'
  { apiId :: Lude.Text,
    typeName :: Lude.Text,
    fieldName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResolver' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'fieldName' - The resolver field name.
-- * 'typeName' - The resolver type name.
mkGetResolver ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  -- | 'fieldName'
  Lude.Text ->
  GetResolver
mkGetResolver pApiId_ pTypeName_ pFieldName_ =
  GetResolver'
    { apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grApiId :: Lens.Lens' GetResolver Lude.Text
grApiId = Lens.lens (apiId :: GetResolver -> Lude.Text) (\s a -> s {apiId = a} :: GetResolver)
{-# DEPRECATED grApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The resolver type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grTypeName :: Lens.Lens' GetResolver Lude.Text
grTypeName = Lens.lens (typeName :: GetResolver -> Lude.Text) (\s a -> s {typeName = a} :: GetResolver)
{-# DEPRECATED grTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grFieldName :: Lens.Lens' GetResolver Lude.Text
grFieldName = Lens.lens (fieldName :: GetResolver -> Lude.Text) (\s a -> s {fieldName = a} :: GetResolver)
{-# DEPRECATED grFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.AWSRequest GetResolver where
  type Rs GetResolver = GetResolverResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResolverResponse'
            Lude.<$> (x Lude..?> "resolver") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResolver where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetResolver where
  toPath GetResolver' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/types/",
        Lude.toBS typeName,
        "/resolvers/",
        Lude.toBS fieldName
      ]

instance Lude.ToQuery GetResolver where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResolverResponse' smart constructor.
data GetResolverResponse = GetResolverResponse'
  { resolver ::
      Lude.Maybe Resolver,
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

-- | Creates a value of 'GetResolverResponse' with the minimum fields required to make a request.
--
-- * 'resolver' - The @Resolver@ object.
-- * 'responseStatus' - The response status code.
mkGetResolverResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResolverResponse
mkGetResolverResponse pResponseStatus_ =
  GetResolverResponse'
    { resolver = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResolver :: Lens.Lens' GetResolverResponse (Lude.Maybe Resolver)
grrsResolver = Lens.lens (resolver :: GetResolverResponse -> Lude.Maybe Resolver) (\s a -> s {resolver = a} :: GetResolverResponse)
{-# DEPRECATED grrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetResolverResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetResolverResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResolverResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Resolver@ object.
module Network.AWS.AppSync.DeleteResolver
  ( -- * Creating a request
    DeleteResolver (..),
    mkDeleteResolver,

    -- ** Request lenses
    drApiId,
    drTypeName,
    drFieldName,

    -- * Destructuring the response
    DeleteResolverResponse (..),
    mkDeleteResolverResponse,

    -- ** Response lenses
    drrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResolver' smart constructor.
data DeleteResolver = DeleteResolver'
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

-- | Creates a value of 'DeleteResolver' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'fieldName' - The resolver field name.
-- * 'typeName' - The name of the resolver type.
mkDeleteResolver ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  -- | 'fieldName'
  Lude.Text ->
  DeleteResolver
mkDeleteResolver pApiId_ pTypeName_ pFieldName_ =
  DeleteResolver'
    { apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drApiId :: Lens.Lens' DeleteResolver Lude.Text
drApiId = Lens.lens (apiId :: DeleteResolver -> Lude.Text) (\s a -> s {apiId = a} :: DeleteResolver)
{-# DEPRECATED drApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the resolver type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTypeName :: Lens.Lens' DeleteResolver Lude.Text
drTypeName = Lens.lens (typeName :: DeleteResolver -> Lude.Text) (\s a -> s {typeName = a} :: DeleteResolver)
{-# DEPRECATED drTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFieldName :: Lens.Lens' DeleteResolver Lude.Text
drFieldName = Lens.lens (fieldName :: DeleteResolver -> Lude.Text) (\s a -> s {fieldName = a} :: DeleteResolver)
{-# DEPRECATED drFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.AWSRequest DeleteResolver where
  type Rs DeleteResolver = DeleteResolverResponse
  request = Req.delete appSyncService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResolverResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResolver where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteResolver where
  toPath DeleteResolver' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/types/",
        Lude.toBS typeName,
        "/resolvers/",
        Lude.toBS fieldName
      ]

instance Lude.ToQuery DeleteResolver where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResolverResponse' smart constructor.
newtype DeleteResolverResponse = DeleteResolverResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResolverResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResolverResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResolverResponse
mkDeleteResolverResponse pResponseStatus_ =
  DeleteResolverResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteResolverResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteResolverResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResolverResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

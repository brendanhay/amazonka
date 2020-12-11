{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a @Function@ .
module Network.AWS.AppSync.GetFunction
  ( -- * Creating a request
    GetFunction (..),
    mkGetFunction,

    -- ** Request lenses
    gfApiId,
    gfFunctionId,

    -- * Destructuring the response
    GetFunctionResponse (..),
    mkGetFunctionResponse,

    -- ** Response lenses
    gfrsFunctionConfiguration,
    gfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunction' smart constructor.
data GetFunction = GetFunction'
  { apiId :: Lude.Text,
    functionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
--
-- * 'apiId' - The GraphQL API ID.
-- * 'functionId' - The @Function@ ID.
mkGetFunction ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'functionId'
  Lude.Text ->
  GetFunction
mkGetFunction pApiId_ pFunctionId_ =
  GetFunction' {apiId = pApiId_, functionId = pFunctionId_}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfApiId :: Lens.Lens' GetFunction Lude.Text
gfApiId = Lens.lens (apiId :: GetFunction -> Lude.Text) (\s a -> s {apiId = a} :: GetFunction)
{-# DEPRECATED gfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFunctionId :: Lens.Lens' GetFunction Lude.Text
gfFunctionId = Lens.lens (functionId :: GetFunction -> Lude.Text) (\s a -> s {functionId = a} :: GetFunction)
{-# DEPRECATED gfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

instance Lude.AWSRequest GetFunction where
  type Rs GetFunction = GetFunctionResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Lude.<$> (x Lude..?> "functionConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetFunction where
  toPath GetFunction' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/functions/", Lude.toBS functionId]

instance Lude.ToQuery GetFunction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { functionConfiguration ::
      Lude.Maybe FunctionConfiguration,
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

-- | Creates a value of 'GetFunctionResponse' with the minimum fields required to make a request.
--
-- * 'functionConfiguration' - The @Function@ object.
-- * 'responseStatus' - The response status code.
mkGetFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFunctionResponse
mkGetFunctionResponse pResponseStatus_ =
  GetFunctionResponse'
    { functionConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFunctionConfiguration :: Lens.Lens' GetFunctionResponse (Lude.Maybe FunctionConfiguration)
gfrsFunctionConfiguration = Lens.lens (functionConfiguration :: GetFunctionResponse -> Lude.Maybe FunctionConfiguration) (\s a -> s {functionConfiguration = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFunctionResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

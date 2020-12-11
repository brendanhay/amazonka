{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetIntrospectionSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the introspection schema for a GraphQL API.
module Network.AWS.AppSync.GetIntrospectionSchema
  ( -- * Creating a request
    GetIntrospectionSchema (..),
    mkGetIntrospectionSchema,

    -- ** Request lenses
    gisIncludeDirectives,
    gisApiId,
    gisFormat,

    -- * Destructuring the response
    GetIntrospectionSchemaResponse (..),
    mkGetIntrospectionSchemaResponse,

    -- ** Response lenses
    gisrsSchema,
    gisrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetIntrospectionSchema' smart constructor.
data GetIntrospectionSchema = GetIntrospectionSchema'
  { includeDirectives ::
      Lude.Maybe Lude.Bool,
    apiId :: Lude.Text,
    format :: OutputType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIntrospectionSchema' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'format' - The schema format: SDL or JSON.
-- * 'includeDirectives' - A flag that specifies whether the schema introspection should contain directives.
mkGetIntrospectionSchema ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'format'
  OutputType ->
  GetIntrospectionSchema
mkGetIntrospectionSchema pApiId_ pFormat_ =
  GetIntrospectionSchema'
    { includeDirectives = Lude.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | A flag that specifies whether the schema introspection should contain directives.
--
-- /Note:/ Consider using 'includeDirectives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisIncludeDirectives :: Lens.Lens' GetIntrospectionSchema (Lude.Maybe Lude.Bool)
gisIncludeDirectives = Lens.lens (includeDirectives :: GetIntrospectionSchema -> Lude.Maybe Lude.Bool) (\s a -> s {includeDirectives = a} :: GetIntrospectionSchema)
{-# DEPRECATED gisIncludeDirectives "Use generic-lens or generic-optics with 'includeDirectives' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisApiId :: Lens.Lens' GetIntrospectionSchema Lude.Text
gisApiId = Lens.lens (apiId :: GetIntrospectionSchema -> Lude.Text) (\s a -> s {apiId = a} :: GetIntrospectionSchema)
{-# DEPRECATED gisApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The schema format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisFormat :: Lens.Lens' GetIntrospectionSchema OutputType
gisFormat = Lens.lens (format :: GetIntrospectionSchema -> OutputType) (\s a -> s {format = a} :: GetIntrospectionSchema)
{-# DEPRECATED gisFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.AWSRequest GetIntrospectionSchema where
  type Rs GetIntrospectionSchema = GetIntrospectionSchemaResponse
  request = Req.get appSyncService
  response =
    Res.receiveBytes
      ( \s h x ->
          GetIntrospectionSchemaResponse'
            Lude.<$> (Lude.pure (Lude.Just x)) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIntrospectionSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetIntrospectionSchema where
  toPath GetIntrospectionSchema' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/schema"]

instance Lude.ToQuery GetIntrospectionSchema where
  toQuery GetIntrospectionSchema' {..} =
    Lude.mconcat
      [ "includeDirectives" Lude.=: includeDirectives,
        "format" Lude.=: format
      ]

-- | /See:/ 'mkGetIntrospectionSchemaResponse' smart constructor.
data GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse'
  { schema ::
      Lude.Maybe Lude.ByteString,
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

-- | Creates a value of 'GetIntrospectionSchemaResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'schema' - The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
mkGetIntrospectionSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIntrospectionSchemaResponse
mkGetIntrospectionSchemaResponse pResponseStatus_ =
  GetIntrospectionSchemaResponse'
    { schema = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsSchema :: Lens.Lens' GetIntrospectionSchemaResponse (Lude.Maybe Lude.ByteString)
gisrsSchema = Lens.lens (schema :: GetIntrospectionSchemaResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {schema = a} :: GetIntrospectionSchemaResponse)
{-# DEPRECATED gisrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetIntrospectionSchemaResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetIntrospectionSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIntrospectionSchemaResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

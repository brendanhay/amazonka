{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.StartSchemaCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new schema to your GraphQL API.
--
-- This operation is asynchronous. Use to determine when it has completed.
module Network.AWS.AppSync.StartSchemaCreation
  ( -- * Creating a request
    StartSchemaCreation (..),
    mkStartSchemaCreation,

    -- ** Request lenses
    sscApiId,
    sscDefinition,

    -- * Destructuring the response
    StartSchemaCreationResponse (..),
    mkStartSchemaCreationResponse,

    -- ** Response lenses
    sscrsStatus,
    sscrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartSchemaCreation' smart constructor.
data StartSchemaCreation = StartSchemaCreation'
  { -- | The API ID.
    apiId :: Lude.Text,
    -- | The schema definition, in GraphQL schema language format.
    definition :: Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSchemaCreation' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'definition' - The schema definition, in GraphQL schema language format.
mkStartSchemaCreation ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'definition'
  Lude.Base64 ->
  StartSchemaCreation
mkStartSchemaCreation pApiId_ pDefinition_ =
  StartSchemaCreation' {apiId = pApiId_, definition = pDefinition_}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscApiId :: Lens.Lens' StartSchemaCreation Lude.Text
sscApiId = Lens.lens (apiId :: StartSchemaCreation -> Lude.Text) (\s a -> s {apiId = a} :: StartSchemaCreation)
{-# DEPRECATED sscApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The schema definition, in GraphQL schema language format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscDefinition :: Lens.Lens' StartSchemaCreation Lude.Base64
sscDefinition = Lens.lens (definition :: StartSchemaCreation -> Lude.Base64) (\s a -> s {definition = a} :: StartSchemaCreation)
{-# DEPRECATED sscDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

instance Lude.AWSRequest StartSchemaCreation where
  type Rs StartSchemaCreation = StartSchemaCreationResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSchemaCreationResponse'
            Lude.<$> (x Lude..?> "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSchemaCreation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSchemaCreation where
  toJSON StartSchemaCreation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("definition" Lude..= definition)])

instance Lude.ToPath StartSchemaCreation where
  toPath StartSchemaCreation' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/schemacreation"]

instance Lude.ToQuery StartSchemaCreation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSchemaCreationResponse' smart constructor.
data StartSchemaCreationResponse = StartSchemaCreationResponse'
  { -- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
    status :: Lude.Maybe SchemaStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSchemaCreationResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
-- * 'responseStatus' - The response status code.
mkStartSchemaCreationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSchemaCreationResponse
mkStartSchemaCreationResponse pResponseStatus_ =
  StartSchemaCreationResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrsStatus :: Lens.Lens' StartSchemaCreationResponse (Lude.Maybe SchemaStatus)
sscrsStatus = Lens.lens (status :: StartSchemaCreationResponse -> Lude.Maybe SchemaStatus) (\s a -> s {status = a} :: StartSchemaCreationResponse)
{-# DEPRECATED sscrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrsResponseStatus :: Lens.Lens' StartSchemaCreationResponse Lude.Int
sscrsResponseStatus = Lens.lens (responseStatus :: StartSchemaCreationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSchemaCreationResponse)
{-# DEPRECATED sscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

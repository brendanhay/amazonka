{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.QuerySchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for the schema version metadata information.
module Network.AWS.Glue.QuerySchemaVersionMetadata
  ( -- * Creating a request
    QuerySchemaVersionMetadata (..),
    mkQuerySchemaVersionMetadata,

    -- ** Request lenses
    qsvmSchemaVersionId,
    qsvmSchemaId,
    qsvmNextToken,
    qsvmMetadataList,
    qsvmSchemaVersionNumber,
    qsvmMaxResults,

    -- * Destructuring the response
    QuerySchemaVersionMetadataResponse (..),
    mkQuerySchemaVersionMetadataResponse,

    -- ** Response lenses
    qsvmrsSchemaVersionId,
    qsvmrsNextToken,
    qsvmrsMetadataInfoMap,
    qsvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkQuerySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { schemaVersionId ::
      Lude.Maybe Lude.Text,
    schemaId :: Lude.Maybe SchemaId,
    nextToken :: Lude.Maybe Lude.Text,
    metadataList ::
      Lude.Maybe [MetadataKeyValuePair],
    schemaVersionNumber ::
      Lude.Maybe SchemaVersionNumber,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QuerySchemaVersionMetadata' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
-- * 'metadataList' - Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'schemaId' - A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
-- * 'schemaVersionId' - The unique version ID of the schema version.
-- * 'schemaVersionNumber' - The version number of the schema.
mkQuerySchemaVersionMetadata ::
  QuerySchemaVersionMetadata
mkQuerySchemaVersionMetadata =
  QuerySchemaVersionMetadata'
    { schemaVersionId = Lude.Nothing,
      schemaId = Lude.Nothing,
      nextToken = Lude.Nothing,
      metadataList = Lude.Nothing,
      schemaVersionNumber = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe Lude.Text)
qsvmSchemaVersionId = Lens.lens (schemaVersionId :: QuerySchemaVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaId :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe SchemaId)
qsvmSchemaId = Lens.lens (schemaId :: QuerySchemaVersionMetadata -> Lude.Maybe SchemaId) (\s a -> s {schemaId = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmNextToken :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe Lude.Text)
qsvmNextToken = Lens.lens (nextToken :: QuerySchemaVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
--
-- /Note:/ Consider using 'metadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMetadataList :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe [MetadataKeyValuePair])
qsvmMetadataList = Lens.lens (metadataList :: QuerySchemaVersionMetadata -> Lude.Maybe [MetadataKeyValuePair]) (\s a -> s {metadataList = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmMetadataList "Use generic-lens or generic-optics with 'metadataList' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionNumber :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe SchemaVersionNumber)
qsvmSchemaVersionNumber = Lens.lens (schemaVersionNumber :: QuerySchemaVersionMetadata -> Lude.Maybe SchemaVersionNumber) (\s a -> s {schemaVersionNumber = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMaxResults :: Lens.Lens' QuerySchemaVersionMetadata (Lude.Maybe Lude.Natural)
qsvmMaxResults = Lens.lens (maxResults :: QuerySchemaVersionMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: QuerySchemaVersionMetadata)
{-# DEPRECATED qsvmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest QuerySchemaVersionMetadata where
  type
    Rs QuerySchemaVersionMetadata =
      QuerySchemaVersionMetadataResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          QuerySchemaVersionMetadataResponse'
            Lude.<$> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MetadataInfoMap" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders QuerySchemaVersionMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.QuerySchemaVersionMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON QuerySchemaVersionMetadata where
  toJSON QuerySchemaVersionMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaVersionId" Lude..=) Lude.<$> schemaVersionId,
            ("SchemaId" Lude..=) Lude.<$> schemaId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MetadataList" Lude..=) Lude.<$> metadataList,
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath QuerySchemaVersionMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery QuerySchemaVersionMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkQuerySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { schemaVersionId ::
      Lude.Maybe Lude.Text,
    nextToken ::
      Lude.Maybe Lude.Text,
    metadataInfoMap ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (MetadataInfo)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QuerySchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- * 'metadataInfoMap' - A map of a metadata key and associated values.
-- * 'nextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
-- * 'responseStatus' - The response status code.
-- * 'schemaVersionId' - The unique version ID of the schema version.
mkQuerySchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  QuerySchemaVersionMetadataResponse
mkQuerySchemaVersionMetadataResponse pResponseStatus_ =
  QuerySchemaVersionMetadataResponse'
    { schemaVersionId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      metadataInfoMap = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrsSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
qsvmrsSchemaVersionId = Lens.lens (schemaVersionId :: QuerySchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadataResponse)
{-# DEPRECATED qsvmrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrsNextToken :: Lens.Lens' QuerySchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
qsvmrsNextToken = Lens.lens (nextToken :: QuerySchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: QuerySchemaVersionMetadataResponse)
{-# DEPRECATED qsvmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A map of a metadata key and associated values.
--
-- /Note:/ Consider using 'metadataInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrsMetadataInfoMap :: Lens.Lens' QuerySchemaVersionMetadataResponse (Lude.Maybe (Lude.HashMap Lude.Text (MetadataInfo)))
qsvmrsMetadataInfoMap = Lens.lens (metadataInfoMap :: QuerySchemaVersionMetadataResponse -> Lude.Maybe (Lude.HashMap Lude.Text (MetadataInfo))) (\s a -> s {metadataInfoMap = a} :: QuerySchemaVersionMetadataResponse)
{-# DEPRECATED qsvmrsMetadataInfoMap "Use generic-lens or generic-optics with 'metadataInfoMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrsResponseStatus :: Lens.Lens' QuerySchemaVersionMetadataResponse Lude.Int
qsvmrsResponseStatus = Lens.lens (responseStatus :: QuerySchemaVersionMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: QuerySchemaVersionMetadataResponse)
{-# DEPRECATED qsvmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.RemoveSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a key value pair from the schema version metadata for the specified schema version ID.
module Network.AWS.Glue.RemoveSchemaVersionMetadata
  ( -- * Creating a request
    RemoveSchemaVersionMetadata (..),
    mkRemoveSchemaVersionMetadata,

    -- ** Request lenses
    rsvmSchemaVersionId,
    rsvmSchemaId,
    rsvmSchemaVersionNumber,
    rsvmMetadataKeyValue,

    -- * Destructuring the response
    RemoveSchemaVersionMetadataResponse (..),
    mkRemoveSchemaVersionMetadataResponse,

    -- ** Response lenses
    rsvmrsRegistryName,
    rsvmrsSchemaName,
    rsvmrsSchemaVersionId,
    rsvmrsVersionNumber,
    rsvmrsSchemaARN,
    rsvmrsMetadataKey,
    rsvmrsMetadataValue,
    rsvmrsLatestVersion,
    rsvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
    schemaId :: Lude.Maybe SchemaId,
    -- | The version number of the schema.
    schemaVersionNumber :: Lude.Maybe SchemaVersionNumber,
    -- | The value of the metadata key.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveSchemaVersionMetadata' with the minimum fields required to make a request.
--
-- * 'schemaVersionId' - The unique version ID of the schema version.
-- * 'schemaId' - A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
-- * 'schemaVersionNumber' - The version number of the schema.
-- * 'metadataKeyValue' - The value of the metadata key.
mkRemoveSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  MetadataKeyValuePair ->
  RemoveSchemaVersionMetadata
mkRemoveSchemaVersionMetadata pMetadataKeyValue_ =
  RemoveSchemaVersionMetadata'
    { schemaVersionId = Lude.Nothing,
      schemaId = Lude.Nothing,
      schemaVersionNumber = Lude.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Lude.Maybe Lude.Text)
rsvmSchemaVersionId = Lens.lens (schemaVersionId :: RemoveSchemaVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadata)
{-# DEPRECATED rsvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaId :: Lens.Lens' RemoveSchemaVersionMetadata (Lude.Maybe SchemaId)
rsvmSchemaId = Lens.lens (schemaId :: RemoveSchemaVersionMetadata -> Lude.Maybe SchemaId) (\s a -> s {schemaId = a} :: RemoveSchemaVersionMetadata)
{-# DEPRECATED rsvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Lude.Maybe SchemaVersionNumber)
rsvmSchemaVersionNumber = Lens.lens (schemaVersionNumber :: RemoveSchemaVersionMetadata -> Lude.Maybe SchemaVersionNumber) (\s a -> s {schemaVersionNumber = a} :: RemoveSchemaVersionMetadata)
{-# DEPRECATED rsvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmMetadataKeyValue :: Lens.Lens' RemoveSchemaVersionMetadata MetadataKeyValuePair
rsvmMetadataKeyValue = Lens.lens (metadataKeyValue :: RemoveSchemaVersionMetadata -> MetadataKeyValuePair) (\s a -> s {metadataKeyValue = a} :: RemoveSchemaVersionMetadata)
{-# DEPRECATED rsvmMetadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead." #-}

instance Lude.AWSRequest RemoveSchemaVersionMetadata where
  type
    Rs RemoveSchemaVersionMetadata =
      RemoveSchemaVersionMetadataResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveSchemaVersionMetadataResponse'
            Lude.<$> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "SchemaName")
            Lude.<*> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "VersionNumber")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (x Lude..?> "MetadataKey")
            Lude.<*> (x Lude..?> "MetadataValue")
            Lude.<*> (x Lude..?> "LatestVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveSchemaVersionMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.RemoveSchemaVersionMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveSchemaVersionMetadata where
  toJSON RemoveSchemaVersionMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaVersionId" Lude..=) Lude.<$> schemaVersionId,
            ("SchemaId" Lude..=) Lude.<$> schemaId,
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber,
            Lude.Just ("MetadataKeyValue" Lude..= metadataKeyValue)
          ]
      )

instance Lude.ToPath RemoveSchemaVersionMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveSchemaVersionMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { -- | The name of the registry.
    registryName :: Lude.Maybe Lude.Text,
    -- | The name of the schema.
    schemaName :: Lude.Maybe Lude.Text,
    -- | The version ID for the schema version.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | The version number of the schema.
    versionNumber :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The metadata key.
    metadataKey :: Lude.Maybe Lude.Text,
    -- | The value of the metadata key.
    metadataValue :: Lude.Maybe Lude.Text,
    -- | The latest version of the schema.
    latestVersion :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveSchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- * 'registryName' - The name of the registry.
-- * 'schemaName' - The name of the schema.
-- * 'schemaVersionId' - The version ID for the schema version.
-- * 'versionNumber' - The version number of the schema.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'metadataKey' - The metadata key.
-- * 'metadataValue' - The value of the metadata key.
-- * 'latestVersion' - The latest version of the schema.
-- * 'responseStatus' - The response status code.
mkRemoveSchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveSchemaVersionMetadataResponse
mkRemoveSchemaVersionMetadataResponse pResponseStatus_ =
  RemoveSchemaVersionMetadataResponse'
    { registryName = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      versionNumber = Lude.Nothing,
      schemaARN = Lude.Nothing,
      metadataKey = Lude.Nothing,
      metadataValue = Lude.Nothing,
      latestVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsRegistryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsRegistryName = Lens.lens (registryName :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsSchemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsSchemaName = Lens.lens (schemaName :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The version ID for the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsSchemaVersionId = Lens.lens (schemaVersionId :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Natural)
rsvmrsVersionNumber = Lens.lens (versionNumber :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsSchemaARN :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsSchemaARN = Lens.lens (schemaARN :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsMetadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsMetadataKey = Lens.lens (metadataKey :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {metadataKey = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsMetadataKey "Use generic-lens or generic-optics with 'metadataKey' instead." #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsMetadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
rsvmrsMetadataValue = Lens.lens (metadataValue :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {metadataValue = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

-- | The latest version of the schema.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsLatestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Lude.Maybe Lude.Bool)
rsvmrsLatestVersion = Lens.lens (latestVersion :: RemoveSchemaVersionMetadataResponse -> Lude.Maybe Lude.Bool) (\s a -> s {latestVersion = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrsResponseStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Lude.Int
rsvmrsResponseStatus = Lens.lens (responseStatus :: RemoveSchemaVersionMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveSchemaVersionMetadataResponse)
{-# DEPRECATED rsvmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the metadata key value pair for a specified schema version ID. A maximum of 10 key value pairs will be allowed per schema version. They can be added over one or more calls.
module Network.AWS.Glue.PutSchemaVersionMetadata
  ( -- * Creating a request
    PutSchemaVersionMetadata (..),
    mkPutSchemaVersionMetadata,

    -- ** Request lenses
    psvmSchemaVersionId,
    psvmSchemaId,
    psvmSchemaVersionNumber,
    psvmMetadataKeyValue,

    -- * Destructuring the response
    PutSchemaVersionMetadataResponse (..),
    mkPutSchemaVersionMetadataResponse,

    -- ** Response lenses
    psvmrsRegistryName,
    psvmrsSchemaName,
    psvmrsSchemaVersionId,
    psvmrsVersionNumber,
    psvmrsSchemaARN,
    psvmrsMetadataKey,
    psvmrsMetadataValue,
    psvmrsLatestVersion,
    psvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | The unique ID for the schema.
    schemaId :: Lude.Maybe SchemaId,
    -- | The version number of the schema.
    schemaVersionNumber :: Lude.Maybe SchemaVersionNumber,
    -- | The metadata key's corresponding value.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSchemaVersionMetadata' with the minimum fields required to make a request.
--
-- * 'schemaVersionId' - The unique version ID of the schema version.
-- * 'schemaId' - The unique ID for the schema.
-- * 'schemaVersionNumber' - The version number of the schema.
-- * 'metadataKeyValue' - The metadata key's corresponding value.
mkPutSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  MetadataKeyValuePair ->
  PutSchemaVersionMetadata
mkPutSchemaVersionMetadata pMetadataKeyValue_ =
  PutSchemaVersionMetadata'
    { schemaVersionId = Lude.Nothing,
      schemaId = Lude.Nothing,
      schemaVersionNumber = Lude.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadata (Lude.Maybe Lude.Text)
psvmSchemaVersionId = Lens.lens (schemaVersionId :: PutSchemaVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: PutSchemaVersionMetadata)
{-# DEPRECATED psvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The unique ID for the schema.
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaId :: Lens.Lens' PutSchemaVersionMetadata (Lude.Maybe SchemaId)
psvmSchemaId = Lens.lens (schemaId :: PutSchemaVersionMetadata -> Lude.Maybe SchemaId) (\s a -> s {schemaId = a} :: PutSchemaVersionMetadata)
{-# DEPRECATED psvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionNumber :: Lens.Lens' PutSchemaVersionMetadata (Lude.Maybe SchemaVersionNumber)
psvmSchemaVersionNumber = Lens.lens (schemaVersionNumber :: PutSchemaVersionMetadata -> Lude.Maybe SchemaVersionNumber) (\s a -> s {schemaVersionNumber = a} :: PutSchemaVersionMetadata)
{-# DEPRECATED psvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

-- | The metadata key's corresponding value.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmMetadataKeyValue :: Lens.Lens' PutSchemaVersionMetadata MetadataKeyValuePair
psvmMetadataKeyValue = Lens.lens (metadataKeyValue :: PutSchemaVersionMetadata -> MetadataKeyValuePair) (\s a -> s {metadataKeyValue = a} :: PutSchemaVersionMetadata)
{-# DEPRECATED psvmMetadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead." #-}

instance Lude.AWSRequest PutSchemaVersionMetadata where
  type Rs PutSchemaVersionMetadata = PutSchemaVersionMetadataResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutSchemaVersionMetadataResponse'
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

instance Lude.ToHeaders PutSchemaVersionMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.PutSchemaVersionMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutSchemaVersionMetadata where
  toJSON PutSchemaVersionMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaVersionId" Lude..=) Lude.<$> schemaVersionId,
            ("SchemaId" Lude..=) Lude.<$> schemaId,
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber,
            Lude.Just ("MetadataKeyValue" Lude..= metadataKeyValue)
          ]
      )

instance Lude.ToPath PutSchemaVersionMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery PutSchemaVersionMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { -- | The name for the registry.
    registryName :: Lude.Maybe Lude.Text,
    -- | The name for the schema.
    schemaName :: Lude.Maybe Lude.Text,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | The version number of the schema.
    versionNumber :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) for the schema.
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

-- | Creates a value of 'PutSchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- * 'registryName' - The name for the registry.
-- * 'schemaName' - The name for the schema.
-- * 'schemaVersionId' - The unique version ID of the schema version.
-- * 'versionNumber' - The version number of the schema.
-- * 'schemaARN' - The Amazon Resource Name (ARN) for the schema.
-- * 'metadataKey' - The metadata key.
-- * 'metadataValue' - The value of the metadata key.
-- * 'latestVersion' - The latest version of the schema.
-- * 'responseStatus' - The response status code.
mkPutSchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutSchemaVersionMetadataResponse
mkPutSchemaVersionMetadataResponse pResponseStatus_ =
  PutSchemaVersionMetadataResponse'
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

-- | The name for the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsRegistryName :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsRegistryName = Lens.lens (registryName :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The name for the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsSchemaName :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsSchemaName = Lens.lens (schemaName :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsSchemaVersionId = Lens.lens (schemaVersionId :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsVersionNumber :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Natural)
psvmrsVersionNumber = Lens.lens (versionNumber :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The Amazon Resource Name (ARN) for the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsSchemaARN :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsSchemaARN = Lens.lens (schemaARN :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsMetadataKey :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsMetadataKey = Lens.lens (metadataKey :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {metadataKey = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsMetadataKey "Use generic-lens or generic-optics with 'metadataKey' instead." #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsMetadataValue :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Text)
psvmrsMetadataValue = Lens.lens (metadataValue :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Text) (\s a -> s {metadataValue = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

-- | The latest version of the schema.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsLatestVersion :: Lens.Lens' PutSchemaVersionMetadataResponse (Lude.Maybe Lude.Bool)
psvmrsLatestVersion = Lens.lens (latestVersion :: PutSchemaVersionMetadataResponse -> Lude.Maybe Lude.Bool) (\s a -> s {latestVersion = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrsResponseStatus :: Lens.Lens' PutSchemaVersionMetadataResponse Lude.Int
psvmrsResponseStatus = Lens.lens (responseStatus :: PutSchemaVersionMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSchemaVersionMetadataResponse)
{-# DEPRECATED psvmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

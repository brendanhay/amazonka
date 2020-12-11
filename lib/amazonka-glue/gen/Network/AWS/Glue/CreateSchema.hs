{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema set and registers the schema definition. Returns an error if the schema set already exists without actually registering the version.
--
-- When the schema set is created, a version checkpoint will be set to the first version. Compatibility mode "DISABLED" restricts any additional schema versions from being added after the first schema version. For all other compatibility modes, validation of compatibility settings will be applied only from the second version onwards when the @RegisterSchemaVersion@ API is used.
-- When this API is called without a @RegistryId@ , this will create an entry for a "default-registry" in the registry database tables, if it is not already present.
module Network.AWS.Glue.CreateSchema
  ( -- * Creating a request
    CreateSchema (..),
    mkCreateSchema,

    -- ** Request lenses
    csSchemaDefinition,
    csRegistryId,
    csDescription,
    csCompatibility,
    csTags,
    csSchemaName,
    csDataFormat,

    -- * Destructuring the response
    CreateSchemaResponse (..),
    mkCreateSchemaResponse,

    -- ** Response lenses
    csrsSchemaVersionStatus,
    csrsRegistryName,
    csrsSchemaStatus,
    csrsRegistryARN,
    csrsLatestSchemaVersion,
    csrsDataFormat,
    csrsSchemaCheckpoint,
    csrsSchemaName,
    csrsSchemaVersionId,
    csrsSchemaARN,
    csrsNextSchemaVersion,
    csrsDescription,
    csrsCompatibility,
    csrsTags,
    csrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { schemaDefinition ::
      Lude.Maybe Lude.Text,
    registryId :: Lude.Maybe RegistryId,
    description :: Lude.Maybe Lude.Text,
    compatibility :: Lude.Maybe Compatibility,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    schemaName :: Lude.Text,
    dataFormat :: DataFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSchema' with the minimum fields required to make a request.
--
-- * 'compatibility' - The compatibility mode of the schema. The possible values are:
--
--
--     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.
--
--
--     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.
--
--
--     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.
--
--
--     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.
--
--
--     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.
--
--
--     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
--
--
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'description' - An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
-- * 'registryId' - This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
-- * 'schemaDefinition' - The schema definition using the @DataFormat@ setting for @SchemaName@ .
-- * 'schemaName' - Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
-- * 'tags' - AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
mkCreateSchema ::
  -- | 'schemaName'
  Lude.Text ->
  -- | 'dataFormat'
  DataFormat ->
  CreateSchema
mkCreateSchema pSchemaName_ pDataFormat_ =
  CreateSchema'
    { schemaDefinition = Lude.Nothing,
      registryId = Lude.Nothing,
      description = Lude.Nothing,
      compatibility = Lude.Nothing,
      tags = Lude.Nothing,
      schemaName = pSchemaName_,
      dataFormat = pDataFormat_
    }

-- | The schema definition using the @DataFormat@ setting for @SchemaName@ .
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSchemaDefinition :: Lens.Lens' CreateSchema (Lude.Maybe Lude.Text)
csSchemaDefinition = Lens.lens (schemaDefinition :: CreateSchema -> Lude.Maybe Lude.Text) (\s a -> s {schemaDefinition = a} :: CreateSchema)
{-# DEPRECATED csSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

-- | This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRegistryId :: Lens.Lens' CreateSchema (Lude.Maybe RegistryId)
csRegistryId = Lens.lens (registryId :: CreateSchema -> Lude.Maybe RegistryId) (\s a -> s {registryId = a} :: CreateSchema)
{-# DEPRECATED csRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSchema (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateSchema -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSchema)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The compatibility mode of the schema. The possible values are:
--
--
--     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.
--
--
--     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.
--
--
--     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.
--
--
--     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.
--
--
--     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.
--
--
--     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.
--
--
--     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
--
--
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompatibility :: Lens.Lens' CreateSchema (Lude.Maybe Compatibility)
csCompatibility = Lens.lens (compatibility :: CreateSchema -> Lude.Maybe Compatibility) (\s a -> s {compatibility = a} :: CreateSchema)
{-# DEPRECATED csCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateSchema (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csTags = Lens.lens (tags :: CreateSchema -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateSchema)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSchemaName :: Lens.Lens' CreateSchema Lude.Text
csSchemaName = Lens.lens (schemaName :: CreateSchema -> Lude.Text) (\s a -> s {schemaName = a} :: CreateSchema)
{-# DEPRECATED csSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDataFormat :: Lens.Lens' CreateSchema DataFormat
csDataFormat = Lens.lens (dataFormat :: CreateSchema -> DataFormat) (\s a -> s {dataFormat = a} :: CreateSchema)
{-# DEPRECATED csDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

instance Lude.AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Lude.<$> (x Lude..?> "SchemaVersionStatus")
            Lude.<*> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "SchemaStatus")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (x Lude..?> "LatestSchemaVersion")
            Lude.<*> (x Lude..?> "DataFormat")
            Lude.<*> (x Lude..?> "SchemaCheckpoint")
            Lude.<*> (x Lude..?> "SchemaName")
            Lude.<*> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (x Lude..?> "NextSchemaVersion")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Compatibility")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateSchema" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaDefinition" Lude..=) Lude.<$> schemaDefinition,
            ("RegistryId" Lude..=) Lude.<$> registryId,
            ("Description" Lude..=) Lude.<$> description,
            ("Compatibility" Lude..=) Lude.<$> compatibility,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("SchemaName" Lude..= schemaName),
            Lude.Just ("DataFormat" Lude..= dataFormat)
          ]
      )

instance Lude.ToPath CreateSchema where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { schemaVersionStatus ::
      Lude.Maybe SchemaVersionStatus,
    registryName :: Lude.Maybe Lude.Text,
    schemaStatus :: Lude.Maybe SchemaStatus,
    registryARN :: Lude.Maybe Lude.Text,
    latestSchemaVersion :: Lude.Maybe Lude.Natural,
    dataFormat :: Lude.Maybe DataFormat,
    schemaCheckpoint :: Lude.Maybe Lude.Natural,
    schemaName :: Lude.Maybe Lude.Text,
    schemaVersionId :: Lude.Maybe Lude.Text,
    schemaARN :: Lude.Maybe Lude.Text,
    nextSchemaVersion :: Lude.Maybe Lude.Natural,
    description :: Lude.Maybe Lude.Text,
    compatibility :: Lude.Maybe Compatibility,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'CreateSchemaResponse' with the minimum fields required to make a request.
--
-- * 'compatibility' - The schema compatibility mode.
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'description' - A description of the schema if specified when created.
-- * 'latestSchemaVersion' - The latest version of the schema associated with the returned schema definition.
-- * 'nextSchemaVersion' - The next version of the schema associated with the returned schema definition.
-- * 'registryARN' - The Amazon Resource Name (ARN) of the registry.
-- * 'registryName' - The name of the registry.
-- * 'responseStatus' - The response status code.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'schemaCheckpoint' - The version number of the checkpoint (the last time the compatibility mode was changed).
-- * 'schemaName' - The name of the schema.
-- * 'schemaStatus' - The status of the schema.
-- * 'schemaVersionId' - The unique identifier of the first schema version.
-- * 'schemaVersionStatus' - The status of the first schema version created.
-- * 'tags' - The tags for the schema.
mkCreateSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSchemaResponse
mkCreateSchemaResponse pResponseStatus_ =
  CreateSchemaResponse'
    { schemaVersionStatus = Lude.Nothing,
      registryName = Lude.Nothing,
      schemaStatus = Lude.Nothing,
      registryARN = Lude.Nothing,
      latestSchemaVersion = Lude.Nothing,
      dataFormat = Lude.Nothing,
      schemaCheckpoint = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      schemaARN = Lude.Nothing,
      nextSchemaVersion = Lude.Nothing,
      description = Lude.Nothing,
      compatibility = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the first schema version created.
--
-- /Note:/ Consider using 'schemaVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaVersionStatus :: Lens.Lens' CreateSchemaResponse (Lude.Maybe SchemaVersionStatus)
csrsSchemaVersionStatus = Lens.lens (schemaVersionStatus :: CreateSchemaResponse -> Lude.Maybe SchemaVersionStatus) (\s a -> s {schemaVersionStatus = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaVersionStatus "Use generic-lens or generic-optics with 'schemaVersionStatus' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsRegistryName :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsRegistryName = Lens.lens (registryName :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaStatus :: Lens.Lens' CreateSchemaResponse (Lude.Maybe SchemaStatus)
csrsSchemaStatus = Lens.lens (schemaStatus :: CreateSchemaResponse -> Lude.Maybe SchemaStatus) (\s a -> s {schemaStatus = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsRegistryARN :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsRegistryARN = Lens.lens (registryARN :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The latest version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'latestSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsLatestSchemaVersion :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Natural)
csrsLatestSchemaVersion = Lens.lens (latestSchemaVersion :: CreateSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {latestSchemaVersion = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsLatestSchemaVersion "Use generic-lens or generic-optics with 'latestSchemaVersion' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsDataFormat :: Lens.Lens' CreateSchemaResponse (Lude.Maybe DataFormat)
csrsDataFormat = Lens.lens (dataFormat :: CreateSchemaResponse -> Lude.Maybe DataFormat) (\s a -> s {dataFormat = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- /Note:/ Consider using 'schemaCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaCheckpoint :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Natural)
csrsSchemaCheckpoint = Lens.lens (schemaCheckpoint :: CreateSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {schemaCheckpoint = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaCheckpoint "Use generic-lens or generic-optics with 'schemaCheckpoint' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaName :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsSchemaName = Lens.lens (schemaName :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The unique identifier of the first schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaVersionId :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsSchemaVersionId = Lens.lens (schemaVersionId :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSchemaARN :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsSchemaARN = Lens.lens (schemaARN :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The next version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'nextSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsNextSchemaVersion :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Natural)
csrsNextSchemaVersion = Lens.lens (nextSchemaVersion :: CreateSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {nextSchemaVersion = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsNextSchemaVersion "Use generic-lens or generic-optics with 'nextSchemaVersion' instead." #-}

-- | A description of the schema if specified when created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsDescription :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Lude.Text)
csrsDescription = Lens.lens (description :: CreateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The schema compatibility mode.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsCompatibility :: Lens.Lens' CreateSchemaResponse (Lude.Maybe Compatibility)
csrsCompatibility = Lens.lens (compatibility :: CreateSchemaResponse -> Lude.Maybe Compatibility) (\s a -> s {compatibility = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | The tags for the schema.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsTags :: Lens.Lens' CreateSchemaResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csrsTags = Lens.lens (tags :: CreateSchemaResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSchemaResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSchemaResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

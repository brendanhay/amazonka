{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified schema in detail.
module Network.AWS.Glue.GetSchema
  ( -- * Creating a request
    GetSchema (..),
    mkGetSchema,

    -- ** Request lenses
    gsSchemaId,

    -- * Destructuring the response
    GetSchemaResponse (..),
    mkGetSchemaResponse,

    -- ** Response lenses
    gsrsRegistryName,
    gsrsCreatedTime,
    gsrsSchemaStatus,
    gsrsRegistryARN,
    gsrsLatestSchemaVersion,
    gsrsDataFormat,
    gsrsSchemaCheckpoint,
    gsrsSchemaName,
    gsrsSchemaARN,
    gsrsNextSchemaVersion,
    gsrsUpdatedTime,
    gsrsDescription,
    gsrsCompatibility,
    gsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchema' smart constructor.
newtype GetSchema = GetSchema' {schemaId :: SchemaId}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchema' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
mkGetSchema ::
  -- | 'schemaId'
  SchemaId ->
  GetSchema
mkGetSchema pSchemaId_ = GetSchema' {schemaId = pSchemaId_}

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSchemaId :: Lens.Lens' GetSchema SchemaId
gsSchemaId = Lens.lens (schemaId :: GetSchema -> SchemaId) (\s a -> s {schemaId = a} :: GetSchema)
{-# DEPRECATED gsSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

instance Lude.AWSRequest GetSchema where
  type Rs GetSchema = GetSchemaResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            Lude.<$> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "CreatedTime")
            Lude.<*> (x Lude..?> "SchemaStatus")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (x Lude..?> "LatestSchemaVersion")
            Lude.<*> (x Lude..?> "DataFormat")
            Lude.<*> (x Lude..?> "SchemaCheckpoint")
            Lude.<*> (x Lude..?> "SchemaName")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (x Lude..?> "NextSchemaVersion")
            Lude.<*> (x Lude..?> "UpdatedTime")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Compatibility")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetSchema" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSchema where
  toJSON GetSchema' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SchemaId" Lude..= schemaId)])

instance Lude.ToPath GetSchema where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { registryName ::
      Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Text,
    schemaStatus :: Lude.Maybe SchemaStatus,
    registryARN :: Lude.Maybe Lude.Text,
    latestSchemaVersion :: Lude.Maybe Lude.Natural,
    dataFormat :: Lude.Maybe DataFormat,
    schemaCheckpoint :: Lude.Maybe Lude.Natural,
    schemaName :: Lude.Maybe Lude.Text,
    schemaARN :: Lude.Maybe Lude.Text,
    nextSchemaVersion :: Lude.Maybe Lude.Natural,
    updatedTime :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    compatibility :: Lude.Maybe Compatibility,
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

-- | Creates a value of 'GetSchemaResponse' with the minimum fields required to make a request.
--
-- * 'compatibility' - The compatibility mode of the schema.
-- * 'createdTime' - The date and time the schema was created.
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'description' - A description of schema if specified when created
-- * 'latestSchemaVersion' - The latest version of the schema associated with the returned schema definition.
-- * 'nextSchemaVersion' - The next version of the schema associated with the returned schema definition.
-- * 'registryARN' - The Amazon Resource Name (ARN) of the registry.
-- * 'registryName' - The name of the registry.
-- * 'responseStatus' - The response status code.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'schemaCheckpoint' - The version number of the checkpoint (the last time the compatibility mode was changed).
-- * 'schemaName' - The name of the schema.
-- * 'schemaStatus' - The status of the schema.
-- * 'updatedTime' - The date and time the schema was updated.
mkGetSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaResponse
mkGetSchemaResponse pResponseStatus_ =
  GetSchemaResponse'
    { registryName = Lude.Nothing,
      createdTime = Lude.Nothing,
      schemaStatus = Lude.Nothing,
      registryARN = Lude.Nothing,
      latestSchemaVersion = Lude.Nothing,
      dataFormat = Lude.Nothing,
      schemaCheckpoint = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaARN = Lude.Nothing,
      nextSchemaVersion = Lude.Nothing,
      updatedTime = Lude.Nothing,
      description = Lude.Nothing,
      compatibility = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsRegistryName :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsRegistryName = Lens.lens (registryName :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The date and time the schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsCreatedTime :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsCreatedTime = Lens.lens (createdTime :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSchemaStatus :: Lens.Lens' GetSchemaResponse (Lude.Maybe SchemaStatus)
gsrsSchemaStatus = Lens.lens (schemaStatus :: GetSchemaResponse -> Lude.Maybe SchemaStatus) (\s a -> s {schemaStatus = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsSchemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsRegistryARN :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsRegistryARN = Lens.lens (registryARN :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The latest version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'latestSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLatestSchemaVersion :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Natural)
gsrsLatestSchemaVersion = Lens.lens (latestSchemaVersion :: GetSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {latestSchemaVersion = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsLatestSchemaVersion "Use generic-lens or generic-optics with 'latestSchemaVersion' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsDataFormat :: Lens.Lens' GetSchemaResponse (Lude.Maybe DataFormat)
gsrsDataFormat = Lens.lens (dataFormat :: GetSchemaResponse -> Lude.Maybe DataFormat) (\s a -> s {dataFormat = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- /Note:/ Consider using 'schemaCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSchemaCheckpoint :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Natural)
gsrsSchemaCheckpoint = Lens.lens (schemaCheckpoint :: GetSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {schemaCheckpoint = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsSchemaCheckpoint "Use generic-lens or generic-optics with 'schemaCheckpoint' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSchemaName :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsSchemaName = Lens.lens (schemaName :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSchemaARN :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsSchemaARN = Lens.lens (schemaARN :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The next version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'nextSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsNextSchemaVersion :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Natural)
gsrsNextSchemaVersion = Lens.lens (nextSchemaVersion :: GetSchemaResponse -> Lude.Maybe Lude.Natural) (\s a -> s {nextSchemaVersion = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsNextSchemaVersion "Use generic-lens or generic-optics with 'nextSchemaVersion' instead." #-}

-- | The date and time the schema was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsUpdatedTime :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsUpdatedTime = Lens.lens (updatedTime :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {updatedTime = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | A description of schema if specified when created
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsDescription :: Lens.Lens' GetSchemaResponse (Lude.Maybe Lude.Text)
gsrsDescription = Lens.lens (description :: GetSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The compatibility mode of the schema.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsCompatibility :: Lens.Lens' GetSchemaResponse (Lude.Maybe Compatibility)
gsrsCompatibility = Lens.lens (compatibility :: GetSchemaResponse -> Lude.Maybe Compatibility) (\s a -> s {compatibility = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetSchemaResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaByDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema by the @SchemaDefinition@ . The schema definition is sent to the Schema Registry, canonicalized, and hashed. If the hash is matched within the scope of the @SchemaName@ or ARN (or the default registry, if none is supplied), that schema’s metadata is returned. Otherwise, a 404 or NotFound error is returned. Schema versions in @Deleted@ statuses will not be included in the results.
module Network.AWS.Glue.GetSchemaByDefinition
  ( -- * Creating a request
    GetSchemaByDefinition (..),
    mkGetSchemaByDefinition,

    -- ** Request lenses
    gsbdSchemaDefinition,
    gsbdSchemaId,

    -- * Destructuring the response
    GetSchemaByDefinitionResponse (..),
    mkGetSchemaByDefinitionResponse,

    -- ** Response lenses
    gsbdrsStatus,
    gsbdrsCreatedTime,
    gsbdrsDataFormat,
    gsbdrsSchemaVersionId,
    gsbdrsSchemaARN,
    gsbdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchemaByDefinition' smart constructor.
data GetSchemaByDefinition = GetSchemaByDefinition'
  { -- | The definition of the schema for which schema details are required.
    schemaDefinition :: Lude.Text,
    -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    schemaId :: SchemaId
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaByDefinition' with the minimum fields required to make a request.
--
-- * 'schemaDefinition' - The definition of the schema for which schema details are required.
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
mkGetSchemaByDefinition ::
  -- | 'schemaDefinition'
  Lude.Text ->
  -- | 'schemaId'
  SchemaId ->
  GetSchemaByDefinition
mkGetSchemaByDefinition pSchemaDefinition_ pSchemaId_ =
  GetSchemaByDefinition'
    { schemaDefinition = pSchemaDefinition_,
      schemaId = pSchemaId_
    }

-- | The definition of the schema for which schema details are required.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdSchemaDefinition :: Lens.Lens' GetSchemaByDefinition Lude.Text
gsbdSchemaDefinition = Lens.lens (schemaDefinition :: GetSchemaByDefinition -> Lude.Text) (\s a -> s {schemaDefinition = a} :: GetSchemaByDefinition)
{-# DEPRECATED gsbdSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdSchemaId :: Lens.Lens' GetSchemaByDefinition SchemaId
gsbdSchemaId = Lens.lens (schemaId :: GetSchemaByDefinition -> SchemaId) (\s a -> s {schemaId = a} :: GetSchemaByDefinition)
{-# DEPRECATED gsbdSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

instance Lude.AWSRequest GetSchemaByDefinition where
  type Rs GetSchemaByDefinition = GetSchemaByDefinitionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaByDefinitionResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "CreatedTime")
            Lude.<*> (x Lude..?> "DataFormat")
            Lude.<*> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchemaByDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetSchemaByDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSchemaByDefinition where
  toJSON GetSchemaByDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaDefinition" Lude..= schemaDefinition),
            Lude.Just ("SchemaId" Lude..= schemaId)
          ]
      )

instance Lude.ToPath GetSchemaByDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSchemaByDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaByDefinitionResponse' smart constructor.
data GetSchemaByDefinitionResponse = GetSchemaByDefinitionResponse'
  { -- | The status of the schema version.
    status :: Lude.Maybe SchemaVersionStatus,
    -- | The date and time the schema was created.
    createdTime :: Lude.Maybe Lude.Text,
    -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Lude.Maybe DataFormat,
    -- | The schema ID of the schema version.
    schemaVersionId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaByDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the schema version.
-- * 'createdTime' - The date and time the schema was created.
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'schemaVersionId' - The schema ID of the schema version.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'responseStatus' - The response status code.
mkGetSchemaByDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaByDefinitionResponse
mkGetSchemaByDefinitionResponse pResponseStatus_ =
  GetSchemaByDefinitionResponse'
    { status = Lude.Nothing,
      createdTime = Lude.Nothing,
      dataFormat = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsStatus :: Lens.Lens' GetSchemaByDefinitionResponse (Lude.Maybe SchemaVersionStatus)
gsbdrsStatus = Lens.lens (status :: GetSchemaByDefinitionResponse -> Lude.Maybe SchemaVersionStatus) (\s a -> s {status = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsCreatedTime :: Lens.Lens' GetSchemaByDefinitionResponse (Lude.Maybe Lude.Text)
gsbdrsCreatedTime = Lens.lens (createdTime :: GetSchemaByDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsDataFormat :: Lens.Lens' GetSchemaByDefinitionResponse (Lude.Maybe DataFormat)
gsbdrsDataFormat = Lens.lens (dataFormat :: GetSchemaByDefinitionResponse -> Lude.Maybe DataFormat) (\s a -> s {dataFormat = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The schema ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsSchemaVersionId :: Lens.Lens' GetSchemaByDefinitionResponse (Lude.Maybe Lude.Text)
gsbdrsSchemaVersionId = Lens.lens (schemaVersionId :: GetSchemaByDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsSchemaARN :: Lens.Lens' GetSchemaByDefinitionResponse (Lude.Maybe Lude.Text)
gsbdrsSchemaARN = Lens.lens (schemaARN :: GetSchemaByDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrsResponseStatus :: Lens.Lens' GetSchemaByDefinitionResponse Lude.Int
gsbdrsResponseStatus = Lens.lens (responseStatus :: GetSchemaByDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaByDefinitionResponse)
{-# DEPRECATED gsbdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

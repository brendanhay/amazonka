{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specified schema by its unique ID assigned when a version of the schema is created or registered. Schema versions in Deleted status will not be included in the results.
module Network.AWS.Glue.GetSchemaVersion
  ( -- * Creating a request
    GetSchemaVersion (..),
    mkGetSchemaVersion,

    -- ** Request lenses
    gsvSchemaVersionId,
    gsvSchemaId,
    gsvSchemaVersionNumber,

    -- * Destructuring the response
    GetSchemaVersionResponse (..),
    mkGetSchemaVersionResponse,

    -- ** Response lenses
    gsvrsStatus,
    gsvrsSchemaDefinition,
    gsvrsCreatedTime,
    gsvrsDataFormat,
    gsvrsSchemaVersionId,
    gsvrsVersionNumber,
    gsvrsSchemaARN,
    gsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { schemaVersionId ::
      Lude.Maybe Lude.Text,
    schemaId :: Lude.Maybe SchemaId,
    schemaVersionNumber :: Lude.Maybe SchemaVersionNumber
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSchemaVersion' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
-- * 'schemaVersionId' - The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
-- * 'schemaVersionNumber' - The version number of the schema.
mkGetSchemaVersion ::
  GetSchemaVersion
mkGetSchemaVersion =
  GetSchemaVersion'
    { schemaVersionId = Lude.Nothing,
      schemaId = Lude.Nothing,
      schemaVersionNumber = Lude.Nothing
    }

-- | The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionId :: Lens.Lens' GetSchemaVersion (Lude.Maybe Lude.Text)
gsvSchemaVersionId = Lens.lens (schemaVersionId :: GetSchemaVersion -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: GetSchemaVersion)
{-# DEPRECATED gsvSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

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
gsvSchemaId :: Lens.Lens' GetSchemaVersion (Lude.Maybe SchemaId)
gsvSchemaId = Lens.lens (schemaId :: GetSchemaVersion -> Lude.Maybe SchemaId) (\s a -> s {schemaId = a} :: GetSchemaVersion)
{-# DEPRECATED gsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionNumber :: Lens.Lens' GetSchemaVersion (Lude.Maybe SchemaVersionNumber)
gsvSchemaVersionNumber = Lens.lens (schemaVersionNumber :: GetSchemaVersion -> Lude.Maybe SchemaVersionNumber) (\s a -> s {schemaVersionNumber = a} :: GetSchemaVersion)
{-# DEPRECATED gsvSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Lude.AWSRequest GetSchemaVersion where
  type Rs GetSchemaVersion = GetSchemaVersionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSchemaVersionResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "SchemaDefinition")
            Lude.<*> (x Lude..?> "CreatedTime")
            Lude.<*> (x Lude..?> "DataFormat")
            Lude.<*> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "VersionNumber")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSchemaVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetSchemaVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSchemaVersion where
  toJSON GetSchemaVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaVersionId" Lude..=) Lude.<$> schemaVersionId,
            ("SchemaId" Lude..=) Lude.<$> schemaId,
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber
          ]
      )

instance Lude.ToPath GetSchemaVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSchemaVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { status ::
      Lude.Maybe SchemaVersionStatus,
    schemaDefinition :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Text,
    dataFormat :: Lude.Maybe DataFormat,
    schemaVersionId :: Lude.Maybe Lude.Text,
    versionNumber :: Lude.Maybe Lude.Natural,
    schemaARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetSchemaVersionResponse' with the minimum fields required to make a request.
--
-- * 'createdTime' - The date and time the schema version was created.
-- * 'dataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
-- * 'responseStatus' - The response status code.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'schemaDefinition' - The schema definition for the schema ID.
-- * 'schemaVersionId' - The @SchemaVersionId@ of the schema version.
-- * 'status' - The status of the schema version.
-- * 'versionNumber' - The version number of the schema.
mkGetSchemaVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSchemaVersionResponse
mkGetSchemaVersionResponse pResponseStatus_ =
  GetSchemaVersionResponse'
    { status = Lude.Nothing,
      schemaDefinition = Lude.Nothing,
      createdTime = Lude.Nothing,
      dataFormat = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      versionNumber = Lude.Nothing,
      schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsStatus :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe SchemaVersionStatus)
gsvrsStatus = Lens.lens (status :: GetSchemaVersionResponse -> Lude.Maybe SchemaVersionStatus) (\s a -> s {status = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The schema definition for the schema ID.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSchemaDefinition :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe Lude.Text)
gsvrsSchemaDefinition = Lens.lens (schemaDefinition :: GetSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaDefinition = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

-- | The date and time the schema version was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsCreatedTime :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe Lude.Text)
gsvrsCreatedTime = Lens.lens (createdTime :: GetSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsDataFormat :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe DataFormat)
gsvrsDataFormat = Lens.lens (dataFormat :: GetSchemaVersionResponse -> Lude.Maybe DataFormat) (\s a -> s {dataFormat = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The @SchemaVersionId@ of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSchemaVersionId :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe Lude.Text)
gsvrsSchemaVersionId = Lens.lens (schemaVersionId :: GetSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsVersionNumber :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe Lude.Natural)
gsvrsVersionNumber = Lens.lens (versionNumber :: GetSchemaVersionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSchemaARN :: Lens.Lens' GetSchemaVersionResponse (Lude.Maybe Lude.Text)
gsvrsSchemaARN = Lens.lens (schemaARN :: GetSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsResponseStatus :: Lens.Lens' GetSchemaVersionResponse Lude.Int
gsvrsResponseStatus = Lens.lens (responseStatus :: GetSchemaVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSchemaVersionResponse)
{-# DEPRECATED gsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

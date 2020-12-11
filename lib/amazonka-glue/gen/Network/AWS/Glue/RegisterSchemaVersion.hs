{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.RegisterSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new version to the existing schema. Returns an error if new version of schema does not meet the compatibility requirements of the schema set. This API will not create a new schema set and will return a 404 error if the schema set is not already present in the Schema Registry.
--
-- If this is the first schema definition to be registered in the Schema Registry, this API will store the schema version and return immediately. Otherwise, this call has the potential to run longer than other operations due to compatibility modes. You can call the @GetSchemaVersion@ API with the @SchemaVersionId@ to check compatibility modes.
-- If the same schema definition is already stored in Schema Registry as a version, the schema ID of the existing schema is returned to the caller.
module Network.AWS.Glue.RegisterSchemaVersion
  ( -- * Creating a request
    RegisterSchemaVersion (..),
    mkRegisterSchemaVersion,

    -- ** Request lenses
    rsvSchemaId,
    rsvSchemaDefinition,

    -- * Destructuring the response
    RegisterSchemaVersionResponse (..),
    mkRegisterSchemaVersionResponse,

    -- ** Response lenses
    rsvrsStatus,
    rsvrsSchemaVersionId,
    rsvrsVersionNumber,
    rsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterSchemaVersion' smart constructor.
data RegisterSchemaVersion = RegisterSchemaVersion'
  { schemaId ::
      SchemaId,
    schemaDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterSchemaVersion' with the minimum fields required to make a request.
--
-- * 'schemaDefinition' - The schema definition using the @DataFormat@ setting for the @SchemaName@ .
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
mkRegisterSchemaVersion ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'schemaDefinition'
  Lude.Text ->
  RegisterSchemaVersion
mkRegisterSchemaVersion pSchemaId_ pSchemaDefinition_ =
  RegisterSchemaVersion'
    { schemaId = pSchemaId_,
      schemaDefinition = pSchemaDefinition_
    }

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
rsvSchemaId :: Lens.Lens' RegisterSchemaVersion SchemaId
rsvSchemaId = Lens.lens (schemaId :: RegisterSchemaVersion -> SchemaId) (\s a -> s {schemaId = a} :: RegisterSchemaVersion)
{-# DEPRECATED rsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The schema definition using the @DataFormat@ setting for the @SchemaName@ .
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvSchemaDefinition :: Lens.Lens' RegisterSchemaVersion Lude.Text
rsvSchemaDefinition = Lens.lens (schemaDefinition :: RegisterSchemaVersion -> Lude.Text) (\s a -> s {schemaDefinition = a} :: RegisterSchemaVersion)
{-# DEPRECATED rsvSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

instance Lude.AWSRequest RegisterSchemaVersion where
  type Rs RegisterSchemaVersion = RegisterSchemaVersionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterSchemaVersionResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "SchemaVersionId")
            Lude.<*> (x Lude..?> "VersionNumber")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterSchemaVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.RegisterSchemaVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterSchemaVersion where
  toJSON RegisterSchemaVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaId" Lude..= schemaId),
            Lude.Just ("SchemaDefinition" Lude..= schemaDefinition)
          ]
      )

instance Lude.ToPath RegisterSchemaVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterSchemaVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterSchemaVersionResponse' smart constructor.
data RegisterSchemaVersionResponse = RegisterSchemaVersionResponse'
  { status ::
      Lude.Maybe SchemaVersionStatus,
    schemaVersionId ::
      Lude.Maybe Lude.Text,
    versionNumber ::
      Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'RegisterSchemaVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'schemaVersionId' - The unique ID that represents the version of this schema.
-- * 'status' - The status of the schema version.
-- * 'versionNumber' - The version of this schema (for sync flow only, in case this is the first version).
mkRegisterSchemaVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterSchemaVersionResponse
mkRegisterSchemaVersionResponse pResponseStatus_ =
  RegisterSchemaVersionResponse'
    { status = Lude.Nothing,
      schemaVersionId = Lude.Nothing,
      versionNumber = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrsStatus :: Lens.Lens' RegisterSchemaVersionResponse (Lude.Maybe SchemaVersionStatus)
rsvrsStatus = Lens.lens (status :: RegisterSchemaVersionResponse -> Lude.Maybe SchemaVersionStatus) (\s a -> s {status = a} :: RegisterSchemaVersionResponse)
{-# DEPRECATED rsvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID that represents the version of this schema.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrsSchemaVersionId :: Lens.Lens' RegisterSchemaVersionResponse (Lude.Maybe Lude.Text)
rsvrsSchemaVersionId = Lens.lens (schemaVersionId :: RegisterSchemaVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersionId = a} :: RegisterSchemaVersionResponse)
{-# DEPRECATED rsvrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version of this schema (for sync flow only, in case this is the first version).
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrsVersionNumber :: Lens.Lens' RegisterSchemaVersionResponse (Lude.Maybe Lude.Natural)
rsvrsVersionNumber = Lens.lens (versionNumber :: RegisterSchemaVersionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: RegisterSchemaVersionResponse)
{-# DEPRECATED rsvrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrsResponseStatus :: Lens.Lens' RegisterSchemaVersionResponse Lude.Int
rsvrsResponseStatus = Lens.lens (responseStatus :: RegisterSchemaVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterSchemaVersionResponse)
{-# DEPRECATED rsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

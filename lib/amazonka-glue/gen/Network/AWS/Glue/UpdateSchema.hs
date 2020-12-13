{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description, compatibility setting, or version checkpoint for a schema set.
--
-- For updating the compatibility setting, the call will not validate compatibility for the entire set of schema versions with the new compatibility setting. If the value for @Compatibility@ is provided, the @VersionNumber@ (a checkpoint) is also required. The API will validate the checkpoint version number for consistency.
-- If the value for the @VersionNumber@ (checkpoint) is provided, @Compatibility@ is optional and this can be used to set/reset a checkpoint for the schema.
-- This update will happen only if the schema is in the AVAILABLE state.
module Network.AWS.Glue.UpdateSchema
  ( -- * Creating a request
    UpdateSchema (..),
    mkUpdateSchema,

    -- ** Request lenses
    usSchemaId,
    usSchemaVersionNumber,
    usDescription,
    usCompatibility,

    -- * Destructuring the response
    UpdateSchemaResponse (..),
    mkUpdateSchemaResponse,

    -- ** Response lenses
    usrsRegistryName,
    usrsSchemaName,
    usrsSchemaARN,
    usrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    schemaId :: SchemaId,
    -- | Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
    schemaVersionNumber :: Lude.Maybe SchemaVersionNumber,
    -- | The new description for the schema.
    description :: Lude.Maybe Lude.Text,
    -- | The new compatibility setting for the schema.
    compatibility :: Lude.Maybe Compatibility
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSchema' with the minimum fields required to make a request.
--
-- * 'schemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
-- * 'schemaVersionNumber' - Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
-- * 'description' - The new description for the schema.
-- * 'compatibility' - The new compatibility setting for the schema.
mkUpdateSchema ::
  -- | 'schemaId'
  SchemaId ->
  UpdateSchema
mkUpdateSchema pSchemaId_ =
  UpdateSchema'
    { schemaId = pSchemaId_,
      schemaVersionNumber = Lude.Nothing,
      description = Lude.Nothing,
      compatibility = Lude.Nothing
    }

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
usSchemaId :: Lens.Lens' UpdateSchema SchemaId
usSchemaId = Lens.lens (schemaId :: UpdateSchema -> SchemaId) (\s a -> s {schemaId = a} :: UpdateSchema)
{-# DEPRECATED usSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaVersionNumber :: Lens.Lens' UpdateSchema (Lude.Maybe SchemaVersionNumber)
usSchemaVersionNumber = Lens.lens (schemaVersionNumber :: UpdateSchema -> Lude.Maybe SchemaVersionNumber) (\s a -> s {schemaVersionNumber = a} :: UpdateSchema)
{-# DEPRECATED usSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

-- | The new description for the schema.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateSchema (Lude.Maybe Lude.Text)
usDescription = Lens.lens (description :: UpdateSchema -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateSchema)
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new compatibility setting for the schema.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCompatibility :: Lens.Lens' UpdateSchema (Lude.Maybe Compatibility)
usCompatibility = Lens.lens (compatibility :: UpdateSchema -> Lude.Maybe Compatibility) (\s a -> s {compatibility = a} :: UpdateSchema)
{-# DEPRECATED usCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

instance Lude.AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Lude.<$> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "SchemaName")
            Lude.<*> (x Lude..?> "SchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateSchema" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaId" Lude..= schemaId),
            ("SchemaVersionNumber" Lude..=) Lude.<$> schemaVersionNumber,
            ("Description" Lude..=) Lude.<$> description,
            ("Compatibility" Lude..=) Lude.<$> compatibility
          ]
      )

instance Lude.ToPath UpdateSchema where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The name of the registry that contains the schema.
    registryName :: Lude.Maybe Lude.Text,
    -- | The name of the schema.
    schemaName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSchemaResponse' with the minimum fields required to make a request.
--
-- * 'registryName' - The name of the registry that contains the schema.
-- * 'schemaName' - The name of the schema.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema.
-- * 'responseStatus' - The response status code.
mkUpdateSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSchemaResponse
mkUpdateSchemaResponse pResponseStatus_ =
  UpdateSchemaResponse'
    { registryName = Lude.Nothing,
      schemaName = Lude.Nothing,
      schemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the registry that contains the schema.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsRegistryName :: Lens.Lens' UpdateSchemaResponse (Lude.Maybe Lude.Text)
usrsRegistryName = Lens.lens (registryName :: UpdateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsSchemaName :: Lens.Lens' UpdateSchemaResponse (Lude.Maybe Lude.Text)
usrsSchemaName = Lens.lens (schemaName :: UpdateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsSchemaARN :: Lens.Lens' UpdateSchemaResponse (Lude.Maybe Lude.Text)
usrsSchemaARN = Lens.lens (schemaARN :: UpdateSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateSchemaResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSchemaResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

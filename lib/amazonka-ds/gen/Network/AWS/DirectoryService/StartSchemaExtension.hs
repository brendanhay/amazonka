{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.StartSchemaExtension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a schema extension to a Microsoft AD directory.
module Network.AWS.DirectoryService.StartSchemaExtension
  ( -- * Creating a request
    StartSchemaExtension (..),
    mkStartSchemaExtension,

    -- ** Request lenses
    sseDirectoryId,
    sseCreateSnapshotBeforeSchemaExtension,
    sseLdifContent,
    sseDescription,

    -- * Destructuring the response
    StartSchemaExtensionResponse (..),
    mkStartSchemaExtensionResponse,

    -- ** Response lenses
    ssersSchemaExtensionId,
    ssersResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartSchemaExtension' smart constructor.
data StartSchemaExtension = StartSchemaExtension'
  { -- | The identifier of the directory for which the schema extension will be applied to.
    directoryId :: Lude.Text,
    -- | If true, creates a snapshot of the directory before applying the schema extension.
    createSnapshotBeforeSchemaExtension :: Lude.Bool,
    -- | The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
    ldifContent :: Lude.Text,
    -- | A description of the schema extension.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSchemaExtension' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which the schema extension will be applied to.
-- * 'createSnapshotBeforeSchemaExtension' - If true, creates a snapshot of the directory before applying the schema extension.
-- * 'ldifContent' - The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
-- * 'description' - A description of the schema extension.
mkStartSchemaExtension ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'createSnapshotBeforeSchemaExtension'
  Lude.Bool ->
  -- | 'ldifContent'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  StartSchemaExtension
mkStartSchemaExtension
  pDirectoryId_
  pCreateSnapshotBeforeSchemaExtension_
  pLdifContent_
  pDescription_ =
    StartSchemaExtension'
      { directoryId = pDirectoryId_,
        createSnapshotBeforeSchemaExtension =
          pCreateSnapshotBeforeSchemaExtension_,
        ldifContent = pLdifContent_,
        description = pDescription_
      }

-- | The identifier of the directory for which the schema extension will be applied to.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseDirectoryId :: Lens.Lens' StartSchemaExtension Lude.Text
sseDirectoryId = Lens.lens (directoryId :: StartSchemaExtension -> Lude.Text) (\s a -> s {directoryId = a} :: StartSchemaExtension)
{-# DEPRECATED sseDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | If true, creates a snapshot of the directory before applying the schema extension.
--
-- /Note:/ Consider using 'createSnapshotBeforeSchemaExtension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseCreateSnapshotBeforeSchemaExtension :: Lens.Lens' StartSchemaExtension Lude.Bool
sseCreateSnapshotBeforeSchemaExtension = Lens.lens (createSnapshotBeforeSchemaExtension :: StartSchemaExtension -> Lude.Bool) (\s a -> s {createSnapshotBeforeSchemaExtension = a} :: StartSchemaExtension)
{-# DEPRECATED sseCreateSnapshotBeforeSchemaExtension "Use generic-lens or generic-optics with 'createSnapshotBeforeSchemaExtension' instead." #-}

-- | The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
--
-- /Note:/ Consider using 'ldifContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseLdifContent :: Lens.Lens' StartSchemaExtension Lude.Text
sseLdifContent = Lens.lens (ldifContent :: StartSchemaExtension -> Lude.Text) (\s a -> s {ldifContent = a} :: StartSchemaExtension)
{-# DEPRECATED sseLdifContent "Use generic-lens or generic-optics with 'ldifContent' instead." #-}

-- | A description of the schema extension.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseDescription :: Lens.Lens' StartSchemaExtension Lude.Text
sseDescription = Lens.lens (description :: StartSchemaExtension -> Lude.Text) (\s a -> s {description = a} :: StartSchemaExtension)
{-# DEPRECATED sseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest StartSchemaExtension where
  type Rs StartSchemaExtension = StartSchemaExtensionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSchemaExtensionResponse'
            Lude.<$> (x Lude..?> "SchemaExtensionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSchemaExtension where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.StartSchemaExtension" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSchemaExtension where
  toJSON StartSchemaExtension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just
              ( "CreateSnapshotBeforeSchemaExtension"
                  Lude..= createSnapshotBeforeSchemaExtension
              ),
            Lude.Just ("LdifContent" Lude..= ldifContent),
            Lude.Just ("Description" Lude..= description)
          ]
      )

instance Lude.ToPath StartSchemaExtension where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSchemaExtension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSchemaExtensionResponse' smart constructor.
data StartSchemaExtensionResponse = StartSchemaExtensionResponse'
  { -- | The identifier of the schema extension that will be applied.
    schemaExtensionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSchemaExtensionResponse' with the minimum fields required to make a request.
--
-- * 'schemaExtensionId' - The identifier of the schema extension that will be applied.
-- * 'responseStatus' - The response status code.
mkStartSchemaExtensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSchemaExtensionResponse
mkStartSchemaExtensionResponse pResponseStatus_ =
  StartSchemaExtensionResponse'
    { schemaExtensionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the schema extension that will be applied.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersSchemaExtensionId :: Lens.Lens' StartSchemaExtensionResponse (Lude.Maybe Lude.Text)
ssersSchemaExtensionId = Lens.lens (schemaExtensionId :: StartSchemaExtensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaExtensionId = a} :: StartSchemaExtensionResponse)
{-# DEPRECATED ssersSchemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssersResponseStatus :: Lens.Lens' StartSchemaExtensionResponse Lude.Int
ssersResponseStatus = Lens.lens (responseStatus :: StartSchemaExtensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSchemaExtensionResponse)
{-# DEPRECATED ssersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

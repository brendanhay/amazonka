{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CancelSchemaExtension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; @Initializing@ , @CreatingSnapshot@ , and @UpdatingSchema@ .
module Network.AWS.DirectoryService.CancelSchemaExtension
  ( -- * Creating a request
    CancelSchemaExtension (..),
    mkCancelSchemaExtension,

    -- ** Request lenses
    cseDirectoryId,
    cseSchemaExtensionId,

    -- * Destructuring the response
    CancelSchemaExtensionResponse (..),
    mkCancelSchemaExtensionResponse,

    -- ** Response lenses
    csersResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelSchemaExtension' smart constructor.
data CancelSchemaExtension = CancelSchemaExtension'
  { directoryId ::
      Lude.Text,
    schemaExtensionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSchemaExtension' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory whose schema extension will be canceled.
-- * 'schemaExtensionId' - The identifier of the schema extension that will be canceled.
mkCancelSchemaExtension ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'schemaExtensionId'
  Lude.Text ->
  CancelSchemaExtension
mkCancelSchemaExtension pDirectoryId_ pSchemaExtensionId_ =
  CancelSchemaExtension'
    { directoryId = pDirectoryId_,
      schemaExtensionId = pSchemaExtensionId_
    }

-- | The identifier of the directory whose schema extension will be canceled.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseDirectoryId :: Lens.Lens' CancelSchemaExtension Lude.Text
cseDirectoryId = Lens.lens (directoryId :: CancelSchemaExtension -> Lude.Text) (\s a -> s {directoryId = a} :: CancelSchemaExtension)
{-# DEPRECATED cseDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the schema extension that will be canceled.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseSchemaExtensionId :: Lens.Lens' CancelSchemaExtension Lude.Text
cseSchemaExtensionId = Lens.lens (schemaExtensionId :: CancelSchemaExtension -> Lude.Text) (\s a -> s {schemaExtensionId = a} :: CancelSchemaExtension)
{-# DEPRECATED cseSchemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead." #-}

instance Lude.AWSRequest CancelSchemaExtension where
  type Rs CancelSchemaExtension = CancelSchemaExtensionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelSchemaExtensionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelSchemaExtension where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.CancelSchemaExtension" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelSchemaExtension where
  toJSON CancelSchemaExtension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("SchemaExtensionId" Lude..= schemaExtensionId)
          ]
      )

instance Lude.ToPath CancelSchemaExtension where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelSchemaExtension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelSchemaExtensionResponse' smart constructor.
newtype CancelSchemaExtensionResponse = CancelSchemaExtensionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSchemaExtensionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelSchemaExtensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelSchemaExtensionResponse
mkCancelSchemaExtensionResponse pResponseStatus_ =
  CancelSchemaExtensionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csersResponseStatus :: Lens.Lens' CancelSchemaExtensionResponse Lude.Int
csersResponseStatus = Lens.lens (responseStatus :: CancelSchemaExtensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelSchemaExtensionResponse)
{-# DEPRECATED csersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

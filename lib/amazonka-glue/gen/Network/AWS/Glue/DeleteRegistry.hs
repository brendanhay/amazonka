{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the entire registry including schema and all of its versions. To get the status of the delete operation, you can call the @GetRegistry@ API after the asynchronous call. Deleting a registry will disable all online operations for the registry such as the @UpdateRegistry@ , @CreateSchema@ , @UpdateSchema@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteRegistry
  ( -- * Creating a request
    DeleteRegistry (..),
    mkDeleteRegistry,

    -- ** Request lenses
    drRegistryId,

    -- * Destructuring the response
    DeleteRegistryResponse (..),
    mkDeleteRegistryResponse,

    -- ** Response lenses
    drrsStatus,
    drrsRegistryName,
    drrsRegistryARN,
    drrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRegistry' smart constructor.
newtype DeleteRegistry = DeleteRegistry' {registryId :: RegistryId}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRegistry' with the minimum fields required to make a request.
--
-- * 'registryId' - This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
mkDeleteRegistry ::
  -- | 'registryId'
  RegistryId ->
  DeleteRegistry
mkDeleteRegistry pRegistryId_ =
  DeleteRegistry' {registryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRegistryId :: Lens.Lens' DeleteRegistry RegistryId
drRegistryId = Lens.lens (registryId :: DeleteRegistry -> RegistryId) (\s a -> s {registryId = a} :: DeleteRegistry)
{-# DEPRECATED drRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Lude.AWSRequest DeleteRegistry where
  type Rs DeleteRegistry = DeleteRegistryResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRegistryResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRegistry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteRegistry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRegistry where
  toJSON DeleteRegistry' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RegistryId" Lude..= registryId)])

instance Lude.ToPath DeleteRegistry where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRegistry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRegistryResponse' smart constructor.
data DeleteRegistryResponse = DeleteRegistryResponse'
  { status ::
      Lude.Maybe RegistryStatus,
    registryName :: Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteRegistryResponse' with the minimum fields required to make a request.
--
-- * 'registryARN' - The Amazon Resource Name (ARN) of the registry being deleted.
-- * 'registryName' - The name of the registry being deleted.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the registry. A successful operation will return the @Deleting@ status.
mkDeleteRegistryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRegistryResponse
mkDeleteRegistryResponse pResponseStatus_ =
  DeleteRegistryResponse'
    { status = Lude.Nothing,
      registryName = Lude.Nothing,
      registryARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the registry. A successful operation will return the @Deleting@ status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsStatus :: Lens.Lens' DeleteRegistryResponse (Lude.Maybe RegistryStatus)
drrsStatus = Lens.lens (status :: DeleteRegistryResponse -> Lude.Maybe RegistryStatus) (\s a -> s {status = a} :: DeleteRegistryResponse)
{-# DEPRECATED drrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the registry being deleted.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRegistryName :: Lens.Lens' DeleteRegistryResponse (Lude.Maybe Lude.Text)
drrsRegistryName = Lens.lens (registryName :: DeleteRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: DeleteRegistryResponse)
{-# DEPRECATED drrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry being deleted.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRegistryARN :: Lens.Lens' DeleteRegistryResponse (Lude.Maybe Lude.Text)
drrsRegistryARN = Lens.lens (registryARN :: DeleteRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: DeleteRegistryResponse)
{-# DEPRECATED drrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRegistryResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRegistryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRegistryResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

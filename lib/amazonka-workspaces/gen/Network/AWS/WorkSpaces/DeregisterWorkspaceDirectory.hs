{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified directory. This operation is asynchronous and returns before the WorkSpace directory is deregistered. If any WorkSpaces are registered to this directory, you must remove them before you can deregister the directory.
module Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
  ( -- * Creating a request
    DeregisterWorkspaceDirectory (..),
    mkDeregisterWorkspaceDirectory,

    -- ** Request lenses
    dwdDirectoryId,

    -- * Destructuring the response
    DeregisterWorkspaceDirectoryResponse (..),
    mkDeregisterWorkspaceDirectoryResponse,

    -- ** Response lenses
    dwdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDeregisterWorkspaceDirectory' smart constructor.
newtype DeregisterWorkspaceDirectory = DeregisterWorkspaceDirectory'
  { -- | The identifier of the directory. If any WorkSpaces are registered to this directory, you must remove them before you deregister the directory, or you will receive an OperationNotSupportedException error.
    directoryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterWorkspaceDirectory' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory. If any WorkSpaces are registered to this directory, you must remove them before you deregister the directory, or you will receive an OperationNotSupportedException error.
mkDeregisterWorkspaceDirectory ::
  -- | 'directoryId'
  Lude.Text ->
  DeregisterWorkspaceDirectory
mkDeregisterWorkspaceDirectory pDirectoryId_ =
  DeregisterWorkspaceDirectory' {directoryId = pDirectoryId_}

-- | The identifier of the directory. If any WorkSpaces are registered to this directory, you must remove them before you deregister the directory, or you will receive an OperationNotSupportedException error.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdDirectoryId :: Lens.Lens' DeregisterWorkspaceDirectory Lude.Text
dwdDirectoryId = Lens.lens (directoryId :: DeregisterWorkspaceDirectory -> Lude.Text) (\s a -> s {directoryId = a} :: DeregisterWorkspaceDirectory)
{-# DEPRECATED dwdDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DeregisterWorkspaceDirectory where
  type
    Rs DeregisterWorkspaceDirectory =
      DeregisterWorkspaceDirectoryResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterWorkspaceDirectoryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterWorkspaceDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DeregisterWorkspaceDirectory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterWorkspaceDirectory where
  toJSON DeregisterWorkspaceDirectory' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath DeregisterWorkspaceDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterWorkspaceDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterWorkspaceDirectoryResponse' smart constructor.
newtype DeregisterWorkspaceDirectoryResponse = DeregisterWorkspaceDirectoryResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterWorkspaceDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterWorkspaceDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterWorkspaceDirectoryResponse
mkDeregisterWorkspaceDirectoryResponse pResponseStatus_ =
  DeregisterWorkspaceDirectoryResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwdrsResponseStatus :: Lens.Lens' DeregisterWorkspaceDirectoryResponse Lude.Int
dwdrsResponseStatus = Lens.lens (responseStatus :: DeregisterWorkspaceDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterWorkspaceDirectoryResponse)
{-# DEPRECATED dwdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

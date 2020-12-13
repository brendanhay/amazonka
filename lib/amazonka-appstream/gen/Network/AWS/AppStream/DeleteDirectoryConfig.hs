{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Directory Config object from AppStream 2.0. This object includes the information required to join streaming instances to an Active Directory domain.
module Network.AWS.AppStream.DeleteDirectoryConfig
  ( -- * Creating a request
    DeleteDirectoryConfig (..),
    mkDeleteDirectoryConfig,

    -- ** Request lenses
    ddcDirectoryName,

    -- * Destructuring the response
    DeleteDirectoryConfigResponse (..),
    mkDeleteDirectoryConfigResponse,

    -- ** Response lenses
    ddcfrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDirectoryConfig' smart constructor.
newtype DeleteDirectoryConfig = DeleteDirectoryConfig'
  { -- | The name of the directory configuration.
    directoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectoryConfig' with the minimum fields required to make a request.
--
-- * 'directoryName' - The name of the directory configuration.
mkDeleteDirectoryConfig ::
  -- | 'directoryName'
  Lude.Text ->
  DeleteDirectoryConfig
mkDeleteDirectoryConfig pDirectoryName_ =
  DeleteDirectoryConfig' {directoryName = pDirectoryName_}

-- | The name of the directory configuration.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDirectoryName :: Lens.Lens' DeleteDirectoryConfig Lude.Text
ddcDirectoryName = Lens.lens (directoryName :: DeleteDirectoryConfig -> Lude.Text) (\s a -> s {directoryName = a} :: DeleteDirectoryConfig)
{-# DEPRECATED ddcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Lude.AWSRequest DeleteDirectoryConfig where
  type Rs DeleteDirectoryConfig = DeleteDirectoryConfigResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDirectoryConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDirectoryConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DeleteDirectoryConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDirectoryConfig where
  toJSON DeleteDirectoryConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DirectoryName" Lude..= directoryName)]
      )

instance Lude.ToPath DeleteDirectoryConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDirectoryConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDirectoryConfigResponse' smart constructor.
newtype DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDirectoryConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDirectoryConfigResponse
mkDeleteDirectoryConfigResponse pResponseStatus_ =
  DeleteDirectoryConfigResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcfrsResponseStatus :: Lens.Lens' DeleteDirectoryConfigResponse Lude.Int
ddcfrsResponseStatus = Lens.lens (responseStatus :: DeleteDirectoryConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectoryConfigResponse)
{-# DEPRECATED ddcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

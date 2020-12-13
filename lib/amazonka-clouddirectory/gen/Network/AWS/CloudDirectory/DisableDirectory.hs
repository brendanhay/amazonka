{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DisableDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.
module Network.AWS.CloudDirectory.DisableDirectory
  ( -- * Creating a request
    DisableDirectory (..),
    mkDisableDirectory,

    -- ** Request lenses
    ddDirectoryARN,

    -- * Destructuring the response
    DisableDirectoryResponse (..),
    mkDisableDirectoryResponse,

    -- ** Response lenses
    drsDirectoryARN,
    drsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableDirectory' smart constructor.
newtype DisableDirectory = DisableDirectory'
  { -- | The ARN of the directory to disable.
    directoryARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDirectory' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory to disable.
mkDisableDirectory ::
  -- | 'directoryARN'
  Lude.Text ->
  DisableDirectory
mkDisableDirectory pDirectoryARN_ =
  DisableDirectory' {directoryARN = pDirectoryARN_}

-- | The ARN of the directory to disable.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryARN :: Lens.Lens' DisableDirectory Lude.Text
ddDirectoryARN = Lens.lens (directoryARN :: DisableDirectory -> Lude.Text) (\s a -> s {directoryARN = a} :: DisableDirectory)
{-# DEPRECATED ddDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

instance Lude.AWSRequest DisableDirectory where
  type Rs DisableDirectory = DisableDirectoryResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisableDirectoryResponse'
            Lude.<$> (x Lude..:> "DirectoryArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableDirectory where
  toHeaders DisableDirectory' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DisableDirectory where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisableDirectory where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/directory/disable"

instance Lude.ToQuery DisableDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableDirectoryResponse' smart constructor.
data DisableDirectoryResponse = DisableDirectoryResponse'
  { -- | The ARN of the directory that has been disabled.
    directoryARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory that has been disabled.
-- * 'responseStatus' - The response status code.
mkDisableDirectoryResponse ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DisableDirectoryResponse
mkDisableDirectoryResponse pDirectoryARN_ pResponseStatus_ =
  DisableDirectoryResponse'
    { directoryARN = pDirectoryARN_,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the directory that has been disabled.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectoryARN :: Lens.Lens' DisableDirectoryResponse Lude.Text
drsDirectoryARN = Lens.lens (directoryARN :: DisableDirectoryResponse -> Lude.Text) (\s a -> s {directoryARN = a} :: DisableDirectoryResponse)
{-# DEPRECATED drsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DisableDirectoryResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DisableDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableDirectoryResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.EnableDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.
module Network.AWS.CloudDirectory.EnableDirectory
  ( -- * Creating a request
    EnableDirectory (..),
    mkEnableDirectory,

    -- ** Request lenses
    edDirectoryARN,

    -- * Destructuring the response
    EnableDirectoryResponse (..),
    mkEnableDirectoryResponse,

    -- ** Response lenses
    edrsDirectoryARN,
    edrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableDirectory' smart constructor.
newtype EnableDirectory = EnableDirectory'
  { -- | The ARN of the directory to enable.
    directoryARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDirectory' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory to enable.
mkEnableDirectory ::
  -- | 'directoryARN'
  Lude.Text ->
  EnableDirectory
mkEnableDirectory pDirectoryARN_ =
  EnableDirectory' {directoryARN = pDirectoryARN_}

-- | The ARN of the directory to enable.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDirectoryARN :: Lens.Lens' EnableDirectory Lude.Text
edDirectoryARN = Lens.lens (directoryARN :: EnableDirectory -> Lude.Text) (\s a -> s {directoryARN = a} :: EnableDirectory)
{-# DEPRECATED edDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

instance Lude.AWSRequest EnableDirectory where
  type Rs EnableDirectory = EnableDirectoryResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          EnableDirectoryResponse'
            Lude.<$> (x Lude..:> "DirectoryArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableDirectory where
  toHeaders EnableDirectory' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON EnableDirectory where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableDirectory where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/directory/enable"

instance Lude.ToQuery EnableDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableDirectoryResponse' smart constructor.
data EnableDirectoryResponse = EnableDirectoryResponse'
  { -- | The ARN of the enabled directory.
    directoryARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the enabled directory.
-- * 'responseStatus' - The response status code.
mkEnableDirectoryResponse ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  EnableDirectoryResponse
mkEnableDirectoryResponse pDirectoryARN_ pResponseStatus_ =
  EnableDirectoryResponse'
    { directoryARN = pDirectoryARN_,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the enabled directory.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edrsDirectoryARN :: Lens.Lens' EnableDirectoryResponse Lude.Text
edrsDirectoryARN = Lens.lens (directoryARN :: EnableDirectoryResponse -> Lude.Text) (\s a -> s {directoryARN = a} :: EnableDirectoryResponse)
{-# DEPRECATED edrsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edrsResponseStatus :: Lens.Lens' EnableDirectoryResponse Lude.Int
edrsResponseStatus = Lens.lens (responseStatus :: EnableDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableDirectoryResponse)
{-# DEPRECATED edrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

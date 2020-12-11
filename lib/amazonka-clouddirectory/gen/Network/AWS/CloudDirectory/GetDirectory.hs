{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about a directory.
module Network.AWS.CloudDirectory.GetDirectory
  ( -- * Creating a request
    GetDirectory (..),
    mkGetDirectory,

    -- ** Request lenses
    gdDirectoryARN,

    -- * Destructuring the response
    GetDirectoryResponse (..),
    mkGetDirectoryResponse,

    -- ** Response lenses
    gdrsResponseStatus,
    gdrsDirectory,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDirectory' smart constructor.
newtype GetDirectory = GetDirectory' {directoryARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDirectory' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory.
mkGetDirectory ::
  -- | 'directoryARN'
  Lude.Text ->
  GetDirectory
mkGetDirectory pDirectoryARN_ =
  GetDirectory' {directoryARN = pDirectoryARN_}

-- | The ARN of the directory.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDirectoryARN :: Lens.Lens' GetDirectory Lude.Text
gdDirectoryARN = Lens.lens (directoryARN :: GetDirectory -> Lude.Text) (\s a -> s {directoryARN = a} :: GetDirectory)
{-# DEPRECATED gdDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

instance Lude.AWSRequest GetDirectory where
  type Rs GetDirectory = GetDirectoryResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDirectoryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Directory")
      )

instance Lude.ToHeaders GetDirectory where
  toHeaders GetDirectory' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON GetDirectory where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetDirectory where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/directory/get"

instance Lude.ToQuery GetDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { responseStatus ::
      Lude.Int,
    directory :: Directory
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directory' - Metadata about the directory.
-- * 'responseStatus' - The response status code.
mkGetDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'directory'
  Directory ->
  GetDirectoryResponse
mkGetDirectoryResponse pResponseStatus_ pDirectory_ =
  GetDirectoryResponse'
    { responseStatus = pResponseStatus_,
      directory = pDirectory_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDirectoryResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDirectoryResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Metadata about the directory.
--
-- /Note:/ Consider using 'directory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDirectory :: Lens.Lens' GetDirectoryResponse Directory
gdrsDirectory = Lens.lens (directory :: GetDirectoryResponse -> Directory) (\s a -> s {directory = a} :: GetDirectoryResponse)
{-# DEPRECATED gdrsDirectory "Use generic-lens or generic-optics with 'directory' instead." #-}

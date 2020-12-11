{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ShareDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a specified directory (@DirectoryId@ ) in your AWS account (directory owner) with another AWS account (directory consumer). With this operation you can use your directory from any AWS account and from any Amazon VPC within an AWS Region.
--
-- When you share your AWS Managed Microsoft AD directory, AWS Directory Service creates a shared directory in the directory consumer account. This shared directory contains the metadata to provide access to the directory within the directory owner account. The shared directory is visible in all VPCs in the directory consumer account.
-- The @ShareMethod@ parameter determines whether the specified directory can be shared between AWS accounts inside the same AWS organization (@ORGANIZATIONS@ ). It also determines whether you can share the directory with any other AWS account either inside or outside of the organization (@HANDSHAKE@ ).
-- The @ShareNotes@ parameter is only used when @HANDSHAKE@ is called, which sends a directory sharing request to the directory consumer.
module Network.AWS.DirectoryService.ShareDirectory
  ( -- * Creating a request
    ShareDirectory (..),
    mkShareDirectory,

    -- ** Request lenses
    sdShareNotes,
    sdDirectoryId,
    sdShareTarget,
    sdShareMethod,

    -- * Destructuring the response
    ShareDirectoryResponse (..),
    mkShareDirectoryResponse,

    -- ** Response lenses
    sdrsSharedDirectoryId,
    sdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkShareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { shareNotes ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    directoryId :: Lude.Text,
    shareTarget :: ShareTarget,
    shareMethod :: ShareMethod
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShareDirectory' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
-- * 'shareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
-- * 'shareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
-- * 'shareTarget' - Identifier for the directory consumer account with whom the directory is to be shared.
mkShareDirectory ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'shareTarget'
  ShareTarget ->
  -- | 'shareMethod'
  ShareMethod ->
  ShareDirectory
mkShareDirectory pDirectoryId_ pShareTarget_ pShareMethod_ =
  ShareDirectory'
    { shareNotes = Lude.Nothing,
      directoryId = pDirectoryId_,
      shareTarget = pShareTarget_,
      shareMethod = pShareMethod_
    }

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareNotes :: Lens.Lens' ShareDirectory (Lude.Maybe (Lude.Sensitive Lude.Text))
sdShareNotes = Lens.lens (shareNotes :: ShareDirectory -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {shareNotes = a} :: ShareDirectory)
{-# DEPRECATED sdShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

-- | Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDirectoryId :: Lens.Lens' ShareDirectory Lude.Text
sdDirectoryId = Lens.lens (directoryId :: ShareDirectory -> Lude.Text) (\s a -> s {directoryId = a} :: ShareDirectory)
{-# DEPRECATED sdDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | Identifier for the directory consumer account with whom the directory is to be shared.
--
-- /Note:/ Consider using 'shareTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareTarget :: Lens.Lens' ShareDirectory ShareTarget
sdShareTarget = Lens.lens (shareTarget :: ShareDirectory -> ShareTarget) (\s a -> s {shareTarget = a} :: ShareDirectory)
{-# DEPRECATED sdShareTarget "Use generic-lens or generic-optics with 'shareTarget' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareMethod :: Lens.Lens' ShareDirectory ShareMethod
sdShareMethod = Lens.lens (shareMethod :: ShareDirectory -> ShareMethod) (\s a -> s {shareMethod = a} :: ShareDirectory)
{-# DEPRECATED sdShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

instance Lude.AWSRequest ShareDirectory where
  type Rs ShareDirectory = ShareDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ShareDirectoryResponse'
            Lude.<$> (x Lude..?> "SharedDirectoryId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ShareDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.ShareDirectory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ShareDirectory where
  toJSON ShareDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShareNotes" Lude..=) Lude.<$> shareNotes,
            Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("ShareTarget" Lude..= shareTarget),
            Lude.Just ("ShareMethod" Lude..= shareMethod)
          ]
      )

instance Lude.ToPath ShareDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery ShareDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkShareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { sharedDirectoryId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ShareDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sharedDirectoryId' - Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
mkShareDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ShareDirectoryResponse
mkShareDirectoryResponse pResponseStatus_ =
  ShareDirectoryResponse'
    { sharedDirectoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsSharedDirectoryId :: Lens.Lens' ShareDirectoryResponse (Lude.Maybe Lude.Text)
sdrsSharedDirectoryId = Lens.lens (sharedDirectoryId :: ShareDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {sharedDirectoryId = a} :: ShareDirectoryResponse)
{-# DEPRECATED sdrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsResponseStatus :: Lens.Lens' ShareDirectoryResponse Lude.Int
sdrsResponseStatus = Lens.lens (responseStatus :: ShareDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ShareDirectoryResponse)
{-# DEPRECATED sdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

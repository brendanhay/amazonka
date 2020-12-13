{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
module Network.AWS.WorkDocs.DeleteFolder
  ( -- * Creating a request
    DeleteFolder (..),
    mkDeleteFolder,

    -- ** Request lenses
    dfAuthenticationToken,
    dfFolderId,

    -- * Destructuring the response
    DeleteFolderResponse (..),
    mkDeleteFolderResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the folder.
    folderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFolder' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
mkDeleteFolder ::
  -- | 'folderId'
  Lude.Text ->
  DeleteFolder
mkDeleteFolder pFolderId_ =
  DeleteFolder'
    { authenticationToken = Lude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAuthenticationToken :: Lens.Lens' DeleteFolder (Lude.Maybe (Lude.Sensitive Lude.Text))
dfAuthenticationToken = Lens.lens (authenticationToken :: DeleteFolder -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteFolder)
{-# DEPRECATED dfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFolderId :: Lens.Lens' DeleteFolder Lude.Text
dfFolderId = Lens.lens (folderId :: DeleteFolder -> Lude.Text) (\s a -> s {folderId = a} :: DeleteFolder)
{-# DEPRECATED dfFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

instance Lude.AWSRequest DeleteFolder where
  type Rs DeleteFolder = DeleteFolderResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeleteFolderResponse'

instance Lude.ToHeaders DeleteFolder where
  toHeaders DeleteFolder' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteFolder where
  toPath DeleteFolder' {..} =
    Lude.mconcat ["/api/v1/folders/", Lude.toBS folderId]

instance Lude.ToQuery DeleteFolder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFolderResponse' with the minimum fields required to make a request.
mkDeleteFolderResponse ::
  DeleteFolderResponse
mkDeleteFolderResponse = DeleteFolderResponse'

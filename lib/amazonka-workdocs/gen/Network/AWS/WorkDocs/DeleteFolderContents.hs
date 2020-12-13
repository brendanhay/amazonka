{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteFolderContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the contents of the specified folder.
module Network.AWS.WorkDocs.DeleteFolderContents
  ( -- * Creating a request
    DeleteFolderContents (..),
    mkDeleteFolderContents,

    -- ** Request lenses
    dfcAuthenticationToken,
    dfcFolderId,

    -- * Destructuring the response
    DeleteFolderContentsResponse (..),
    mkDeleteFolderContentsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteFolderContents' smart constructor.
data DeleteFolderContents = DeleteFolderContents'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the folder.
    folderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFolderContents' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
mkDeleteFolderContents ::
  -- | 'folderId'
  Lude.Text ->
  DeleteFolderContents
mkDeleteFolderContents pFolderId_ =
  DeleteFolderContents'
    { authenticationToken = Lude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcAuthenticationToken :: Lens.Lens' DeleteFolderContents (Lude.Maybe (Lude.Sensitive Lude.Text))
dfcAuthenticationToken = Lens.lens (authenticationToken :: DeleteFolderContents -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteFolderContents)
{-# DEPRECATED dfcAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFolderId :: Lens.Lens' DeleteFolderContents Lude.Text
dfcFolderId = Lens.lens (folderId :: DeleteFolderContents -> Lude.Text) (\s a -> s {folderId = a} :: DeleteFolderContents)
{-# DEPRECATED dfcFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

instance Lude.AWSRequest DeleteFolderContents where
  type Rs DeleteFolderContents = DeleteFolderContentsResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeleteFolderContentsResponse'

instance Lude.ToHeaders DeleteFolderContents where
  toHeaders DeleteFolderContents' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteFolderContents where
  toPath DeleteFolderContents' {..} =
    Lude.mconcat
      ["/api/v1/folders/", Lude.toBS folderId, "/contents"]

instance Lude.ToQuery DeleteFolderContents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse = DeleteFolderContentsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFolderContentsResponse' with the minimum fields required to make a request.
mkDeleteFolderContentsResponse ::
  DeleteFolderContentsResponse
mkDeleteFolderContentsResponse = DeleteFolderContentsResponse'

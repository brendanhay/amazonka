{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a folder with the specified name and parent folder.
module Network.AWS.WorkDocs.CreateFolder
  ( -- * Creating a request
    CreateFolder (..),
    mkCreateFolder,

    -- ** Request lenses
    cfAuthenticationToken,
    cfName,
    cfParentFolderId,

    -- * Destructuring the response
    CreateFolderResponse (..),
    mkCreateFolderResponse,

    -- ** Response lenses
    cfrsMetadata,
    cfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateFolder' smart constructor.
data CreateFolder = CreateFolder'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    parentFolderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFolder' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'name' - The name of the new folder.
-- * 'parentFolderId' - The ID of the parent folder.
mkCreateFolder ::
  -- | 'parentFolderId'
  Lude.Text ->
  CreateFolder
mkCreateFolder pParentFolderId_ =
  CreateFolder'
    { authenticationToken = Lude.Nothing,
      name = Lude.Nothing,
      parentFolderId = pParentFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAuthenticationToken :: Lens.Lens' CreateFolder (Lude.Maybe (Lude.Sensitive Lude.Text))
cfAuthenticationToken = Lens.lens (authenticationToken :: CreateFolder -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: CreateFolder)
{-# DEPRECATED cfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the new folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFolder (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: CreateFolder -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateFolder)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfParentFolderId :: Lens.Lens' CreateFolder Lude.Text
cfParentFolderId = Lens.lens (parentFolderId :: CreateFolder -> Lude.Text) (\s a -> s {parentFolderId = a} :: CreateFolder)
{-# DEPRECATED cfParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

instance Lude.AWSRequest CreateFolder where
  type Rs CreateFolder = CreateFolderResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFolderResponse'
            Lude.<$> (x Lude..?> "Metadata") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFolder where
  toHeaders CreateFolder' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateFolder where
  toJSON CreateFolder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("ParentFolderId" Lude..= parentFolderId)
          ]
      )

instance Lude.ToPath CreateFolder where
  toPath = Lude.const "/api/v1/folders"

instance Lude.ToQuery CreateFolder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { metadata ::
      Lude.Maybe FolderMetadata,
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

-- | Creates a value of 'CreateFolderResponse' with the minimum fields required to make a request.
--
-- * 'metadata' - The metadata of the folder.
-- * 'responseStatus' - The response status code.
mkCreateFolderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFolderResponse
mkCreateFolderResponse pResponseStatus_ =
  CreateFolderResponse'
    { metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metadata of the folder.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsMetadata :: Lens.Lens' CreateFolderResponse (Lude.Maybe FolderMetadata)
cfrsMetadata = Lens.lens (metadata :: CreateFolderResponse -> Lude.Maybe FolderMetadata) (\s a -> s {metadata = a} :: CreateFolderResponse)
{-# DEPRECATED cfrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFolderResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFolderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFolderResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

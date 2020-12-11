{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateFolder
  ( -- * Creating a request
    UpdateFolder (..),
    mkUpdateFolder,

    -- ** Request lenses
    ufParentFolderId,
    ufAuthenticationToken,
    ufName,
    ufResourceState,
    ufFolderId,

    -- * Destructuring the response
    UpdateFolderResponse (..),
    mkUpdateFolderResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { parentFolderId ::
      Lude.Maybe Lude.Text,
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    resourceState :: Lude.Maybe ResourceStateType,
    folderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFolder' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'folderId' - The ID of the folder.
-- * 'name' - The name of the folder.
-- * 'parentFolderId' - The ID of the parent folder.
-- * 'resourceState' - The resource state of the folder. Only ACTIVE and RECYCLED are accepted values from the API.
mkUpdateFolder ::
  -- | 'folderId'
  Lude.Text ->
  UpdateFolder
mkUpdateFolder pFolderId_ =
  UpdateFolder'
    { parentFolderId = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      name = Lude.Nothing,
      resourceState = Lude.Nothing,
      folderId = pFolderId_
    }

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufParentFolderId :: Lens.Lens' UpdateFolder (Lude.Maybe Lude.Text)
ufParentFolderId = Lens.lens (parentFolderId :: UpdateFolder -> Lude.Maybe Lude.Text) (\s a -> s {parentFolderId = a} :: UpdateFolder)
{-# DEPRECATED ufParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAuthenticationToken :: Lens.Lens' UpdateFolder (Lude.Maybe (Lude.Sensitive Lude.Text))
ufAuthenticationToken = Lens.lens (authenticationToken :: UpdateFolder -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: UpdateFolder)
{-# DEPRECATED ufAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFolder (Lude.Maybe Lude.Text)
ufName = Lens.lens (name :: UpdateFolder -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateFolder)
{-# DEPRECATED ufName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted values from the API.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResourceState :: Lens.Lens' UpdateFolder (Lude.Maybe ResourceStateType)
ufResourceState = Lens.lens (resourceState :: UpdateFolder -> Lude.Maybe ResourceStateType) (\s a -> s {resourceState = a} :: UpdateFolder)
{-# DEPRECATED ufResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFolderId :: Lens.Lens' UpdateFolder Lude.Text
ufFolderId = Lens.lens (folderId :: UpdateFolder -> Lude.Text) (\s a -> s {folderId = a} :: UpdateFolder)
{-# DEPRECATED ufFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

instance Lude.AWSRequest UpdateFolder where
  type Rs UpdateFolder = UpdateFolderResponse
  request = Req.patchJSON workDocsService
  response = Res.receiveNull UpdateFolderResponse'

instance Lude.ToHeaders UpdateFolder where
  toHeaders UpdateFolder' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON UpdateFolder where
  toJSON UpdateFolder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentFolderId" Lude..=) Lude.<$> parentFolderId,
            ("Name" Lude..=) Lude.<$> name,
            ("ResourceState" Lude..=) Lude.<$> resourceState
          ]
      )

instance Lude.ToPath UpdateFolder where
  toPath UpdateFolder' {..} =
    Lude.mconcat ["/api/v1/folders/", Lude.toBS folderId]

instance Lude.ToQuery UpdateFolder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFolderResponse' with the minimum fields required to make a request.
mkUpdateFolderResponse ::
  UpdateFolderResponse
mkUpdateFolderResponse = UpdateFolderResponse'

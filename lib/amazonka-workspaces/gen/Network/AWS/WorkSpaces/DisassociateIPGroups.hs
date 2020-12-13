{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DisassociateIPGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified IP access control group from the specified directory.
module Network.AWS.WorkSpaces.DisassociateIPGroups
  ( -- * Creating a request
    DisassociateIPGroups (..),
    mkDisassociateIPGroups,

    -- ** Request lenses
    dipgDirectoryId,
    dipgGroupIds,

    -- * Destructuring the response
    DisassociateIPGroupsResponse (..),
    mkDisassociateIPGroupsResponse,

    -- ** Response lenses
    dipgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDisassociateIPGroups' smart constructor.
data DisassociateIPGroups = DisassociateIPGroups'
  { -- | The identifier of the directory.
    directoryId :: Lude.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateIPGroups' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'groupIds' - The identifiers of one or more IP access control groups.
mkDisassociateIPGroups ::
  -- | 'directoryId'
  Lude.Text ->
  DisassociateIPGroups
mkDisassociateIPGroups pDirectoryId_ =
  DisassociateIPGroups'
    { directoryId = pDirectoryId_,
      groupIds = Lude.mempty
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipgDirectoryId :: Lens.Lens' DisassociateIPGroups Lude.Text
dipgDirectoryId = Lens.lens (directoryId :: DisassociateIPGroups -> Lude.Text) (\s a -> s {directoryId = a} :: DisassociateIPGroups)
{-# DEPRECATED dipgDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipgGroupIds :: Lens.Lens' DisassociateIPGroups [Lude.Text]
dipgGroupIds = Lens.lens (groupIds :: DisassociateIPGroups -> [Lude.Text]) (\s a -> s {groupIds = a} :: DisassociateIPGroups)
{-# DEPRECATED dipgGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

instance Lude.AWSRequest DisassociateIPGroups where
  type Rs DisassociateIPGroups = DisassociateIPGroupsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateIPGroupsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateIPGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DisassociateIpGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateIPGroups where
  toJSON DisassociateIPGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("GroupIds" Lude..= groupIds)
          ]
      )

instance Lude.ToPath DisassociateIPGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateIPGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateIPGroupsResponse' smart constructor.
newtype DisassociateIPGroupsResponse = DisassociateIPGroupsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateIPGroupsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateIPGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateIPGroupsResponse
mkDisassociateIPGroupsResponse pResponseStatus_ =
  DisassociateIPGroupsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipgrsResponseStatus :: Lens.Lens' DisassociateIPGroupsResponse Lude.Int
dipgrsResponseStatus = Lens.lens (responseStatus :: DisassociateIPGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateIPGroupsResponse)
{-# DEPRECATED dipgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

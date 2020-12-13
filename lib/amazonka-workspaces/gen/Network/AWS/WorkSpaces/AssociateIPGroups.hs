{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AssociateIPGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified IP access control group with the specified directory.
module Network.AWS.WorkSpaces.AssociateIPGroups
  ( -- * Creating a request
    AssociateIPGroups (..),
    mkAssociateIPGroups,

    -- ** Request lenses
    aigDirectoryId,
    aigGroupIds,

    -- * Destructuring the response
    AssociateIPGroupsResponse (..),
    mkAssociateIPGroupsResponse,

    -- ** Response lenses
    aigrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkAssociateIPGroups' smart constructor.
data AssociateIPGroups = AssociateIPGroups'
  { -- | The identifier of the directory.
    directoryId :: Lude.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateIPGroups' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'groupIds' - The identifiers of one or more IP access control groups.
mkAssociateIPGroups ::
  -- | 'directoryId'
  Lude.Text ->
  AssociateIPGroups
mkAssociateIPGroups pDirectoryId_ =
  AssociateIPGroups'
    { directoryId = pDirectoryId_,
      groupIds = Lude.mempty
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigDirectoryId :: Lens.Lens' AssociateIPGroups Lude.Text
aigDirectoryId = Lens.lens (directoryId :: AssociateIPGroups -> Lude.Text) (\s a -> s {directoryId = a} :: AssociateIPGroups)
{-# DEPRECATED aigDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigGroupIds :: Lens.Lens' AssociateIPGroups [Lude.Text]
aigGroupIds = Lens.lens (groupIds :: AssociateIPGroups -> [Lude.Text]) (\s a -> s {groupIds = a} :: AssociateIPGroups)
{-# DEPRECATED aigGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

instance Lude.AWSRequest AssociateIPGroups where
  type Rs AssociateIPGroups = AssociateIPGroupsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateIPGroupsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateIPGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.AssociateIpGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateIPGroups where
  toJSON AssociateIPGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("GroupIds" Lude..= groupIds)
          ]
      )

instance Lude.ToPath AssociateIPGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateIPGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateIPGroupsResponse' smart constructor.
newtype AssociateIPGroupsResponse = AssociateIPGroupsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateIPGroupsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateIPGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateIPGroupsResponse
mkAssociateIPGroupsResponse pResponseStatus_ =
  AssociateIPGroupsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrsResponseStatus :: Lens.Lens' AssociateIPGroupsResponse Lude.Int
aigrsResponseStatus = Lens.lens (responseStatus :: AssociateIPGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateIPGroupsResponse)
{-# DEPRECATED aigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

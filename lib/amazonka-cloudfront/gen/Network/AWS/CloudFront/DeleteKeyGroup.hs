{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key group.
--
-- You cannot delete a key group that is referenced in a cache behavior. First update your distributions to remove the key group from all cache behaviors, then delete the key group.
-- To delete a key group, you must provide the key group’s identifier and version. To get these values, use @ListKeyGroups@ followed by @GetKeyGroup@ or @GetKeyGroupConfig@ .
module Network.AWS.CloudFront.DeleteKeyGroup
  ( -- * Creating a request
    DeleteKeyGroup (..),
    mkDeleteKeyGroup,

    -- ** Request lenses
    dkgIfMatch,
    dkgId,

    -- * Destructuring the response
    DeleteKeyGroupResponse (..),
    mkDeleteKeyGroupResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteKeyGroup' smart constructor.
data DeleteKeyGroup = DeleteKeyGroup'
  { -- | The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyGroup' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
-- * 'id' - The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
mkDeleteKeyGroup ::
  -- | 'id'
  Lude.Text ->
  DeleteKeyGroup
mkDeleteKeyGroup pId_ =
  DeleteKeyGroup' {ifMatch = Lude.Nothing, id = pId_}

-- | The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkgIfMatch :: Lens.Lens' DeleteKeyGroup (Lude.Maybe Lude.Text)
dkgIfMatch = Lens.lens (ifMatch :: DeleteKeyGroup -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteKeyGroup)
{-# DEPRECATED dkgIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkgId :: Lens.Lens' DeleteKeyGroup Lude.Text
dkgId = Lens.lens (id :: DeleteKeyGroup -> Lude.Text) (\s a -> s {id = a} :: DeleteKeyGroup)
{-# DEPRECATED dkgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteKeyGroup where
  type Rs DeleteKeyGroup = DeleteKeyGroupResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeleteKeyGroupResponse'

instance Lude.ToHeaders DeleteKeyGroup where
  toHeaders DeleteKeyGroup' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteKeyGroup where
  toPath DeleteKeyGroup' {..} =
    Lude.mconcat ["/2020-05-31/key-group/", Lude.toBS id]

instance Lude.ToQuery DeleteKeyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteKeyGroupResponse' smart constructor.
data DeleteKeyGroupResponse = DeleteKeyGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteKeyGroupResponse' with the minimum fields required to make a request.
mkDeleteKeyGroupResponse ::
  DeleteKeyGroupResponse
mkDeleteKeyGroupResponse = DeleteKeyGroupResponse'

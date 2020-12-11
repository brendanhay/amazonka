{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache subnet group.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
  ( -- * Creating a request
    DeleteCacheSubnetGroup (..),
    mkDeleteCacheSubnetGroup,

    -- ** Request lenses
    dCacheSubnetGroupName,

    -- * Destructuring the response
    DeleteCacheSubnetGroupResponse (..),
    mkDeleteCacheSubnetGroupResponse,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteCacheSubnetGroup@ operation.
--
-- /See:/ 'mkDeleteCacheSubnetGroup' smart constructor.
newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup'
  { cacheSubnetGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheSubnetGroup' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroupName' - The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
mkDeleteCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Lude.Text ->
  DeleteCacheSubnetGroup
mkDeleteCacheSubnetGroup pCacheSubnetGroupName_ =
  DeleteCacheSubnetGroup'
    { cacheSubnetGroupName =
        pCacheSubnetGroupName_
    }

-- | The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheSubnetGroupName :: Lens.Lens' DeleteCacheSubnetGroup Lude.Text
dCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: DeleteCacheSubnetGroup -> Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: DeleteCacheSubnetGroup)
{-# DEPRECATED dCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

instance Lude.AWSRequest DeleteCacheSubnetGroup where
  type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse
  request = Req.postQuery elastiCacheService
  response = Res.receiveNull DeleteCacheSubnetGroupResponse'

instance Lude.ToHeaders DeleteCacheSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCacheSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCacheSubnetGroup where
  toQuery DeleteCacheSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCacheSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName
      ]

-- | /See:/ 'mkDeleteCacheSubnetGroupResponse' smart constructor.
data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheSubnetGroupResponse' with the minimum fields required to make a request.
mkDeleteCacheSubnetGroupResponse ::
  DeleteCacheSubnetGroupResponse
mkDeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'

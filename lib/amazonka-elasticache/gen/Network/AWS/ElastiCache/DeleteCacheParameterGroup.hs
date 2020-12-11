{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
  ( -- * Creating a request
    DeleteCacheParameterGroup (..),
    mkDeleteCacheParameterGroup,

    -- ** Request lenses
    dCacheParameterGroupName,

    -- * Destructuring the response
    DeleteCacheParameterGroupResponse (..),
    mkDeleteCacheParameterGroupResponse,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteCacheParameterGroup@ operation.
--
-- /See:/ 'mkDeleteCacheParameterGroup' smart constructor.
newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup'
  { cacheParameterGroupName ::
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

-- | Creates a value of 'DeleteCacheParameterGroup' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of the cache parameter group to delete.
mkDeleteCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Lude.Text ->
  DeleteCacheParameterGroup
mkDeleteCacheParameterGroup pCacheParameterGroupName_ =
  DeleteCacheParameterGroup'
    { cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | The name of the cache parameter group to delete.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheParameterGroupName :: Lens.Lens' DeleteCacheParameterGroup Lude.Text
dCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: DeleteCacheParameterGroup -> Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: DeleteCacheParameterGroup)
{-# DEPRECATED dCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

instance Lude.AWSRequest DeleteCacheParameterGroup where
  type
    Rs DeleteCacheParameterGroup =
      DeleteCacheParameterGroupResponse
  request = Req.postQuery elastiCacheService
  response = Res.receiveNull DeleteCacheParameterGroupResponse'

instance Lude.ToHeaders DeleteCacheParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCacheParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCacheParameterGroup where
  toQuery DeleteCacheParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCacheParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName
      ]

-- | /See:/ 'mkDeleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheParameterGroupResponse' with the minimum fields required to make a request.
mkDeleteCacheParameterGroupResponse ::
  DeleteCacheParameterGroupResponse
mkDeleteCacheParameterGroupResponse =
  DeleteCacheParameterGroupResponse'

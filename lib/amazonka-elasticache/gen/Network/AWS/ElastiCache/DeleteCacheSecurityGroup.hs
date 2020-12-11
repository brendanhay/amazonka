{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache security group.
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
  ( -- * Creating a request
    DeleteCacheSecurityGroup (..),
    mkDeleteCacheSecurityGroup,

    -- ** Request lenses
    dcsgCacheSecurityGroupName,

    -- * Destructuring the response
    DeleteCacheSecurityGroupResponse (..),
    mkDeleteCacheSecurityGroupResponse,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
-- /See:/ 'mkDeleteCacheSecurityGroup' smart constructor.
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { cacheSecurityGroupName ::
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

-- | Creates a value of 'DeleteCacheSecurityGroup' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroupName' - The name of the cache security group to delete.
mkDeleteCacheSecurityGroup ::
  -- | 'cacheSecurityGroupName'
  Lude.Text ->
  DeleteCacheSecurityGroup
mkDeleteCacheSecurityGroup pCacheSecurityGroupName_ =
  DeleteCacheSecurityGroup'
    { cacheSecurityGroupName =
        pCacheSecurityGroupName_
    }

-- | The name of the cache security group to delete.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgCacheSecurityGroupName :: Lens.Lens' DeleteCacheSecurityGroup Lude.Text
dcsgCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: DeleteCacheSecurityGroup -> Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: DeleteCacheSecurityGroup)
{-# DEPRECATED dcsgCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

instance Lude.AWSRequest DeleteCacheSecurityGroup where
  type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse
  request = Req.postQuery elastiCacheService
  response = Res.receiveNull DeleteCacheSecurityGroupResponse'

instance Lude.ToHeaders DeleteCacheSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCacheSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCacheSecurityGroup where
  toQuery DeleteCacheSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCacheSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSecurityGroupName" Lude.=: cacheSecurityGroupName
      ]

-- | /See:/ 'mkDeleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheSecurityGroupResponse' with the minimum fields required to make a request.
mkDeleteCacheSecurityGroupResponse ::
  DeleteCacheSecurityGroupResponse
mkDeleteCacheSecurityGroupResponse =
  DeleteCacheSecurityGroupResponse'

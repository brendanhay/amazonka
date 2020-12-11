{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing cache subnet group.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
  ( -- * Creating a request
    ModifyCacheSubnetGroup (..),
    mkModifyCacheSubnetGroup,

    -- ** Request lenses
    mcsgSubnetIds,
    mcsgCacheSubnetGroupDescription,
    mcsgCacheSubnetGroupName,

    -- * Destructuring the response
    ModifyCacheSubnetGroupResponse (..),
    mkModifyCacheSubnetGroupResponse,

    -- ** Response lenses
    mcsgrsCacheSubnetGroup,
    mcsgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
-- /See:/ 'mkModifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { subnetIds ::
      Lude.Maybe [Lude.Text],
    cacheSubnetGroupDescription ::
      Lude.Maybe Lude.Text,
    cacheSubnetGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCacheSubnetGroup' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroupDescription' - A description of the cache subnet group.
-- * 'cacheSubnetGroupName' - The name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@
-- * 'subnetIds' - The EC2 subnet IDs for the cache subnet group.
mkModifyCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Lude.Text ->
  ModifyCacheSubnetGroup
mkModifyCacheSubnetGroup pCacheSubnetGroupName_ =
  ModifyCacheSubnetGroup'
    { subnetIds = Lude.Nothing,
      cacheSubnetGroupDescription = Lude.Nothing,
      cacheSubnetGroupName = pCacheSubnetGroupName_
    }

-- | The EC2 subnet IDs for the cache subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyCacheSubnetGroup (Lude.Maybe [Lude.Text])
mcsgSubnetIds = Lens.lens (subnetIds :: ModifyCacheSubnetGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: ModifyCacheSubnetGroup)
{-# DEPRECATED mcsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A description of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupDescription :: Lens.Lens' ModifyCacheSubnetGroup (Lude.Maybe Lude.Text)
mcsgCacheSubnetGroupDescription = Lens.lens (cacheSubnetGroupDescription :: ModifyCacheSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupDescription = a} :: ModifyCacheSubnetGroup)
{-# DEPRECATED mcsgCacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead." #-}

-- | The name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupName :: Lens.Lens' ModifyCacheSubnetGroup Lude.Text
mcsgCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: ModifyCacheSubnetGroup -> Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: ModifyCacheSubnetGroup)
{-# DEPRECATED mcsgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

instance Lude.AWSRequest ModifyCacheSubnetGroup where
  type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyCacheSubnetGroupResult"
      ( \s h x ->
          ModifyCacheSubnetGroupResponse'
            Lude.<$> (x Lude..@? "CacheSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCacheSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCacheSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCacheSubnetGroup where
  toQuery ModifyCacheSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCacheSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "SubnetIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SubnetIdentifier" Lude.<$> subnetIds),
        "CacheSubnetGroupDescription" Lude.=: cacheSubnetGroupDescription,
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName
      ]

-- | /See:/ 'mkModifyCacheSubnetGroupResponse' smart constructor.
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
  { cacheSubnetGroup ::
      Lude.Maybe CacheSubnetGroup,
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

-- | Creates a value of 'ModifyCacheSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyCacheSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyCacheSubnetGroupResponse
mkModifyCacheSubnetGroupResponse pResponseStatus_ =
  ModifyCacheSubnetGroupResponse'
    { cacheSubnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrsCacheSubnetGroup :: Lens.Lens' ModifyCacheSubnetGroupResponse (Lude.Maybe CacheSubnetGroup)
mcsgrsCacheSubnetGroup = Lens.lens (cacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> Lude.Maybe CacheSubnetGroup) (\s a -> s {cacheSubnetGroup = a} :: ModifyCacheSubnetGroupResponse)
{-# DEPRECATED mcsgrsCacheSubnetGroup "Use generic-lens or generic-optics with 'cacheSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrsResponseStatus :: Lens.Lens' ModifyCacheSubnetGroupResponse Lude.Int
mcsgrsResponseStatus = Lens.lens (responseStatus :: ModifyCacheSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyCacheSubnetGroupResponse)
{-# DEPRECATED mcsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

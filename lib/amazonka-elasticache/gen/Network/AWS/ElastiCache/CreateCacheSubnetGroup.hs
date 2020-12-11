{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache subnet group.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
  ( -- * Creating a request
    CreateCacheSubnetGroup (..),
    mkCreateCacheSubnetGroup,

    -- ** Request lenses
    ccsgCacheSubnetGroupName,
    ccsgCacheSubnetGroupDescription,
    ccsgSubnetIds,

    -- * Destructuring the response
    CreateCacheSubnetGroupResponse (..),
    mkCreateCacheSubnetGroupResponse,

    -- ** Response lenses
    crsCacheSubnetGroup,
    crsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateCacheSubnetGroup@ operation.
--
-- /See:/ 'mkCreateCacheSubnetGroup' smart constructor.
data CreateCacheSubnetGroup = CreateCacheSubnetGroup'
  { cacheSubnetGroupName ::
      Lude.Text,
    cacheSubnetGroupDescription :: Lude.Text,
    subnetIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheSubnetGroup' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroupDescription' - A description for the cache subnet group.
-- * 'cacheSubnetGroupName' - A name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@
-- * 'subnetIds' - A list of VPC subnet IDs for the cache subnet group.
mkCreateCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Lude.Text ->
  -- | 'cacheSubnetGroupDescription'
  Lude.Text ->
  CreateCacheSubnetGroup
mkCreateCacheSubnetGroup
  pCacheSubnetGroupName_
  pCacheSubnetGroupDescription_ =
    CreateCacheSubnetGroup'
      { cacheSubnetGroupName =
          pCacheSubnetGroupName_,
        cacheSubnetGroupDescription = pCacheSubnetGroupDescription_,
        subnetIds = Lude.mempty
      }

-- | A name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSubnetGroupName :: Lens.Lens' CreateCacheSubnetGroup Lude.Text
ccsgCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: CreateCacheSubnetGroup -> Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: CreateCacheSubnetGroup)
{-# DEPRECATED ccsgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | A description for the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSubnetGroupDescription :: Lens.Lens' CreateCacheSubnetGroup Lude.Text
ccsgCacheSubnetGroupDescription = Lens.lens (cacheSubnetGroupDescription :: CreateCacheSubnetGroup -> Lude.Text) (\s a -> s {cacheSubnetGroupDescription = a} :: CreateCacheSubnetGroup)
{-# DEPRECATED ccsgCacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead." #-}

-- | A list of VPC subnet IDs for the cache subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgSubnetIds :: Lens.Lens' CreateCacheSubnetGroup [Lude.Text]
ccsgSubnetIds = Lens.lens (subnetIds :: CreateCacheSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateCacheSubnetGroup)
{-# DEPRECATED ccsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.AWSRequest CreateCacheSubnetGroup where
  type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateCacheSubnetGroupResult"
      ( \s h x ->
          CreateCacheSubnetGroupResponse'
            Lude.<$> (x Lude..@? "CacheSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCacheSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCacheSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCacheSubnetGroup where
  toQuery CreateCacheSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCacheSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName,
        "CacheSubnetGroupDescription" Lude.=: cacheSubnetGroupDescription,
        "SubnetIds" Lude.=: Lude.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'mkCreateCacheSubnetGroupResponse' smart constructor.
data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse'
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

-- | Creates a value of 'CreateCacheSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateCacheSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCacheSubnetGroupResponse
mkCreateCacheSubnetGroupResponse pResponseStatus_ =
  CreateCacheSubnetGroupResponse'
    { cacheSubnetGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCacheSubnetGroup :: Lens.Lens' CreateCacheSubnetGroupResponse (Lude.Maybe CacheSubnetGroup)
crsCacheSubnetGroup = Lens.lens (cacheSubnetGroup :: CreateCacheSubnetGroupResponse -> Lude.Maybe CacheSubnetGroup) (\s a -> s {cacheSubnetGroup = a} :: CreateCacheSubnetGroupResponse)
{-# DEPRECATED crsCacheSubnetGroup "Use generic-lens or generic-optics with 'cacheSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateCacheSubnetGroupResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateCacheSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCacheSubnetGroupResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

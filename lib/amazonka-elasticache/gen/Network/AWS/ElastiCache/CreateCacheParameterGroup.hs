{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ElastiCache cache parameter group. An ElastiCache cache parameter group is a collection of parameters and their values that are applied to all of the nodes in any cluster or replication group using the CacheParameterGroup.
--
-- A newly created CacheParameterGroup is an exact duplicate of the default parameter group for the CacheParameterGroupFamily. To customize the newly created CacheParameterGroup you can change the values of specific parameters. For more information, see:
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html ModifyCacheParameterGroup> in the ElastiCache API Reference.
--
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ParameterGroups.html Parameters and Parameter Groups> in the ElastiCache User Guide.
module Network.AWS.ElastiCache.CreateCacheParameterGroup
  ( -- * Creating a request
    CreateCacheParameterGroup (..),
    mkCreateCacheParameterGroup,

    -- ** Request lenses
    ccpgCacheParameterGroupFamily,
    ccpgCacheParameterGroupName,
    ccpgDescription,

    -- * Destructuring the response
    CreateCacheParameterGroupResponse (..),
    mkCreateCacheParameterGroupResponse,

    -- ** Response lenses
    ccpgrsCacheParameterGroup,
    ccpgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateCacheParameterGroup@ operation.
--
-- /See:/ 'mkCreateCacheParameterGroup' smart constructor.
data CreateCacheParameterGroup = CreateCacheParameterGroup'
  { -- | The name of the cache parameter group family that the cache parameter group can be used with.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
    cacheParameterGroupFamily :: Lude.Text,
    -- | A user-specified name for the cache parameter group.
    cacheParameterGroupName :: Lude.Text,
    -- | A user-specified description for the cache parameter group.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheParameterGroup' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupFamily' - The name of the cache parameter group family that the cache parameter group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
-- * 'cacheParameterGroupName' - A user-specified name for the cache parameter group.
-- * 'description' - A user-specified description for the cache parameter group.
mkCreateCacheParameterGroup ::
  -- | 'cacheParameterGroupFamily'
  Lude.Text ->
  -- | 'cacheParameterGroupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateCacheParameterGroup
mkCreateCacheParameterGroup
  pCacheParameterGroupFamily_
  pCacheParameterGroupName_
  pDescription_ =
    CreateCacheParameterGroup'
      { cacheParameterGroupFamily =
          pCacheParameterGroupFamily_,
        cacheParameterGroupName = pCacheParameterGroupName_,
        description = pDescription_
      }

-- | The name of the cache parameter group family that the cache parameter group can be used with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgCacheParameterGroupFamily :: Lens.Lens' CreateCacheParameterGroup Lude.Text
ccpgCacheParameterGroupFamily = Lens.lens (cacheParameterGroupFamily :: CreateCacheParameterGroup -> Lude.Text) (\s a -> s {cacheParameterGroupFamily = a} :: CreateCacheParameterGroup)
{-# DEPRECATED ccpgCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | A user-specified name for the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgCacheParameterGroupName :: Lens.Lens' CreateCacheParameterGroup Lude.Text
ccpgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: CreateCacheParameterGroup -> Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: CreateCacheParameterGroup)
{-# DEPRECATED ccpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A user-specified description for the cache parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgDescription :: Lens.Lens' CreateCacheParameterGroup Lude.Text
ccpgDescription = Lens.lens (description :: CreateCacheParameterGroup -> Lude.Text) (\s a -> s {description = a} :: CreateCacheParameterGroup)
{-# DEPRECATED ccpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateCacheParameterGroup where
  type
    Rs CreateCacheParameterGroup =
      CreateCacheParameterGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateCacheParameterGroupResult"
      ( \s h x ->
          CreateCacheParameterGroupResponse'
            Lude.<$> (x Lude..@? "CacheParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCacheParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCacheParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCacheParameterGroup where
  toQuery CreateCacheParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCacheParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupFamily" Lude.=: cacheParameterGroupFamily,
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkCreateCacheParameterGroupResponse' smart constructor.
data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse'
  { cacheParameterGroup :: Lude.Maybe CacheParameterGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroup' -
-- * 'responseStatus' - The response status code.
mkCreateCacheParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCacheParameterGroupResponse
mkCreateCacheParameterGroupResponse pResponseStatus_ =
  CreateCacheParameterGroupResponse'
    { cacheParameterGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrsCacheParameterGroup :: Lens.Lens' CreateCacheParameterGroupResponse (Lude.Maybe CacheParameterGroup)
ccpgrsCacheParameterGroup = Lens.lens (cacheParameterGroup :: CreateCacheParameterGroupResponse -> Lude.Maybe CacheParameterGroup) (\s a -> s {cacheParameterGroup = a} :: CreateCacheParameterGroupResponse)
{-# DEPRECATED ccpgrsCacheParameterGroup "Use generic-lens or generic-optics with 'cacheParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpgrsResponseStatus :: Lens.Lens' CreateCacheParameterGroupResponse Lude.Int
ccpgrsResponseStatus = Lens.lens (responseStatus :: CreateCacheParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCacheParameterGroupResponse)
{-# DEPRECATED ccpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

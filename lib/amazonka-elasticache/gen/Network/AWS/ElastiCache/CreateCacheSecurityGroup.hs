{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache security group. Use a cache security group to control access to one or more clusters.
--
-- Cache security groups are only used when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC). If you are creating a cluster inside of a VPC, use a cache subnet group instead. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html CreateCacheSubnetGroup> .
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
  ( -- * Creating a request
    CreateCacheSecurityGroup (..),
    mkCreateCacheSecurityGroup,

    -- ** Request lenses
    ccsgCacheSecurityGroupName,
    ccsgDescription,

    -- * Destructuring the response
    CreateCacheSecurityGroupResponse (..),
    mkCreateCacheSecurityGroupResponse,

    -- ** Response lenses
    ccsgrsCacheSecurityGroup,
    ccsgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateCacheSecurityGroup@ operation.
--
-- /See:/ 'mkCreateCacheSecurityGroup' smart constructor.
data CreateCacheSecurityGroup = CreateCacheSecurityGroup'
  { -- | A name for the cache security group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters. Cannot be the word "Default".
    -- Example: @mysecuritygroup@
    cacheSecurityGroupName :: Lude.Text,
    -- | A description for the cache security group.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheSecurityGroup' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroupName' - A name for the cache security group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Cannot be the word "Default".
-- Example: @mysecuritygroup@
-- * 'description' - A description for the cache security group.
mkCreateCacheSecurityGroup ::
  -- | 'cacheSecurityGroupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateCacheSecurityGroup
mkCreateCacheSecurityGroup pCacheSecurityGroupName_ pDescription_ =
  CreateCacheSecurityGroup'
    { cacheSecurityGroupName =
        pCacheSecurityGroupName_,
      description = pDescription_
    }

-- | A name for the cache security group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Cannot be the word "Default".
-- Example: @mysecuritygroup@
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgCacheSecurityGroupName :: Lens.Lens' CreateCacheSecurityGroup Lude.Text
ccsgCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: CreateCacheSecurityGroup -> Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: CreateCacheSecurityGroup)
{-# DEPRECATED ccsgCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | A description for the cache security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgDescription :: Lens.Lens' CreateCacheSecurityGroup Lude.Text
ccsgDescription = Lens.lens (description :: CreateCacheSecurityGroup -> Lude.Text) (\s a -> s {description = a} :: CreateCacheSecurityGroup)
{-# DEPRECATED ccsgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateCacheSecurityGroup where
  type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateCacheSecurityGroupResult"
      ( \s h x ->
          CreateCacheSecurityGroupResponse'
            Lude.<$> (x Lude..@? "CacheSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCacheSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCacheSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCacheSecurityGroup where
  toQuery CreateCacheSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCacheSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSecurityGroupName" Lude.=: cacheSecurityGroupName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkCreateCacheSecurityGroupResponse' smart constructor.
data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse'
  { cacheSecurityGroup :: Lude.Maybe CacheSecurityGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroup' -
-- * 'responseStatus' - The response status code.
mkCreateCacheSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCacheSecurityGroupResponse
mkCreateCacheSecurityGroupResponse pResponseStatus_ =
  CreateCacheSecurityGroupResponse'
    { cacheSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrsCacheSecurityGroup :: Lens.Lens' CreateCacheSecurityGroupResponse (Lude.Maybe CacheSecurityGroup)
ccsgrsCacheSecurityGroup = Lens.lens (cacheSecurityGroup :: CreateCacheSecurityGroupResponse -> Lude.Maybe CacheSecurityGroup) (\s a -> s {cacheSecurityGroup = a} :: CreateCacheSecurityGroupResponse)
{-# DEPRECATED ccsgrsCacheSecurityGroup "Use generic-lens or generic-optics with 'cacheSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsgrsResponseStatus :: Lens.Lens' CreateCacheSecurityGroupResponse Lude.Int
ccsgrsResponseStatus = Lens.lens (responseStatus :: CreateCacheSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCacheSecurityGroupResponse)
{-# DEPRECATED ccsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

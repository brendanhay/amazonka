{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Global Datastore for Redis offers fully managed, fast, reliable and secure cross-region replication. Using Global Datastore for Redis, you can create cross-region read replica clusters for ElastiCache for Redis to enable low-latency reads and disaster recovery across regions. For more information, see </AmazonElastiCache/latest/red-ug/Redis-Global-Clusters.html Replication Across Regions Using Global Datastore> .
--
--
--     * The __GlobalReplicationGroupIdSuffix__ is the name of the Global Datastore.
--
--
--     * The __PrimaryReplicationGroupId__ represents the name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
module Network.AWS.ElastiCache.CreateGlobalReplicationGroup
  ( -- * Creating a request
    CreateGlobalReplicationGroup (..),
    mkCreateGlobalReplicationGroup,

    -- ** Request lenses
    cgrgPrimaryReplicationGroupId,
    cgrgGlobalReplicationGroupIdSuffix,
    cgrgGlobalReplicationGroupDescription,

    -- * Destructuring the response
    CreateGlobalReplicationGroupResponse (..),
    mkCreateGlobalReplicationGroupResponse,

    -- ** Response lenses
    cgrgrsGlobalReplicationGroup,
    cgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGlobalReplicationGroup' smart constructor.
data CreateGlobalReplicationGroup = CreateGlobalReplicationGroup'
  { -- | The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
    primaryReplicationGroupId :: Lude.Text,
    -- | The suffix name of a Global Datastore. Amazon ElastiCache automatically applies a prefix to the Global Datastore ID when it is created. Each AWS Region has its own prefix. For instance, a Global Datastore ID created in the US-West-1 region will begin with "dsdfu" along with the suffix name you provide. The suffix, combined with the auto-generated prefix, guarantees uniqueness of the Global Datastore name across multiple regions.
    --
    -- For a full list of AWS Regions and their respective Global Datastore iD prefixes, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Clusters-CLI.html Using the AWS CLI with Global Datastores > .
    globalReplicationGroupIdSuffix :: Lude.Text,
    -- | Provides details of the Global Datastore
    globalReplicationGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'primaryReplicationGroupId' - The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
-- * 'globalReplicationGroupIdSuffix' - The suffix name of a Global Datastore. Amazon ElastiCache automatically applies a prefix to the Global Datastore ID when it is created. Each AWS Region has its own prefix. For instance, a Global Datastore ID created in the US-West-1 region will begin with "dsdfu" along with the suffix name you provide. The suffix, combined with the auto-generated prefix, guarantees uniqueness of the Global Datastore name across multiple regions.
--
-- For a full list of AWS Regions and their respective Global Datastore iD prefixes, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Clusters-CLI.html Using the AWS CLI with Global Datastores > .
-- * 'globalReplicationGroupDescription' - Provides details of the Global Datastore
mkCreateGlobalReplicationGroup ::
  -- | 'primaryReplicationGroupId'
  Lude.Text ->
  -- | 'globalReplicationGroupIdSuffix'
  Lude.Text ->
  CreateGlobalReplicationGroup
mkCreateGlobalReplicationGroup
  pPrimaryReplicationGroupId_
  pGlobalReplicationGroupIdSuffix_ =
    CreateGlobalReplicationGroup'
      { primaryReplicationGroupId =
          pPrimaryReplicationGroupId_,
        globalReplicationGroupIdSuffix = pGlobalReplicationGroupIdSuffix_,
        globalReplicationGroupDescription = Lude.Nothing
      }

-- | The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
--
-- /Note:/ Consider using 'primaryReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgPrimaryReplicationGroupId :: Lens.Lens' CreateGlobalReplicationGroup Lude.Text
cgrgPrimaryReplicationGroupId = Lens.lens (primaryReplicationGroupId :: CreateGlobalReplicationGroup -> Lude.Text) (\s a -> s {primaryReplicationGroupId = a} :: CreateGlobalReplicationGroup)
{-# DEPRECATED cgrgPrimaryReplicationGroupId "Use generic-lens or generic-optics with 'primaryReplicationGroupId' instead." #-}

-- | The suffix name of a Global Datastore. Amazon ElastiCache automatically applies a prefix to the Global Datastore ID when it is created. Each AWS Region has its own prefix. For instance, a Global Datastore ID created in the US-West-1 region will begin with "dsdfu" along with the suffix name you provide. The suffix, combined with the auto-generated prefix, guarantees uniqueness of the Global Datastore name across multiple regions.
--
-- For a full list of AWS Regions and their respective Global Datastore iD prefixes, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Clusters-CLI.html Using the AWS CLI with Global Datastores > .
--
-- /Note:/ Consider using 'globalReplicationGroupIdSuffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgGlobalReplicationGroupIdSuffix :: Lens.Lens' CreateGlobalReplicationGroup Lude.Text
cgrgGlobalReplicationGroupIdSuffix = Lens.lens (globalReplicationGroupIdSuffix :: CreateGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupIdSuffix = a} :: CreateGlobalReplicationGroup)
{-# DEPRECATED cgrgGlobalReplicationGroupIdSuffix "Use generic-lens or generic-optics with 'globalReplicationGroupIdSuffix' instead." #-}

-- | Provides details of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgGlobalReplicationGroupDescription :: Lens.Lens' CreateGlobalReplicationGroup (Lude.Maybe Lude.Text)
cgrgGlobalReplicationGroupDescription = Lens.lens (globalReplicationGroupDescription :: CreateGlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupDescription = a} :: CreateGlobalReplicationGroup)
{-# DEPRECATED cgrgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

instance Lude.AWSRequest CreateGlobalReplicationGroup where
  type
    Rs CreateGlobalReplicationGroup =
      CreateGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateGlobalReplicationGroupResult"
      ( \s h x ->
          CreateGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGlobalReplicationGroup where
  toQuery CreateGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "PrimaryReplicationGroupId" Lude.=: primaryReplicationGroupId,
        "GlobalReplicationGroupIdSuffix"
          Lude.=: globalReplicationGroupIdSuffix,
        "GlobalReplicationGroupDescription"
          Lude.=: globalReplicationGroupDescription
      ]

-- | /See:/ 'mkCreateGlobalReplicationGroupResponse' smart constructor.
data CreateGlobalReplicationGroupResponse = CreateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkCreateGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGlobalReplicationGroupResponse
mkCreateGlobalReplicationGroupResponse pResponseStatus_ =
  CreateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgrsGlobalReplicationGroup :: Lens.Lens' CreateGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
cgrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: CreateGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: CreateGlobalReplicationGroupResponse)
{-# DEPRECATED cgrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgrsResponseStatus :: Lens.Lens' CreateGlobalReplicationGroupResponse Lude.Int
cgrgrsResponseStatus = Lens.lens (responseStatus :: CreateGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGlobalReplicationGroupResponse)
{-# DEPRECATED cgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication subnet group given a list of the subnet IDs in a VPC.
module Network.AWS.DMS.CreateReplicationSubnetGroup
  ( -- * Creating a request
    CreateReplicationSubnetGroup (..),
    mkCreateReplicationSubnetGroup,

    -- ** Request lenses
    crsgSubnetIds,
    crsgReplicationSubnetGroupIdentifier,
    crsgReplicationSubnetGroupDescription,
    crsgTags,

    -- * Destructuring the response
    CreateReplicationSubnetGroupResponse (..),
    mkCreateReplicationSubnetGroupResponse,

    -- ** Response lenses
    crsgrsReplicationSubnetGroup,
    crsgrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { -- | One or more subnet IDs to be assigned to the subnet group.
    subnetIds :: [Lude.Text],
    -- | The name for the replication subnet group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
    -- Example: @mySubnetgroup@
    replicationSubnetGroupIdentifier :: Lude.Text,
    -- | The description for the subnet group.
    replicationSubnetGroupDescription :: Lude.Text,
    -- | One or more tags to be assigned to the subnet group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- * 'subnetIds' - One or more subnet IDs to be assigned to the subnet group.
-- * 'replicationSubnetGroupIdentifier' - The name for the replication subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
-- Example: @mySubnetgroup@
-- * 'replicationSubnetGroupDescription' - The description for the subnet group.
-- * 'tags' - One or more tags to be assigned to the subnet group.
mkCreateReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Lude.Text ->
  -- | 'replicationSubnetGroupDescription'
  Lude.Text ->
  CreateReplicationSubnetGroup
mkCreateReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_
  pReplicationSubnetGroupDescription_ =
    CreateReplicationSubnetGroup'
      { subnetIds = Lude.mempty,
        replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_,
        replicationSubnetGroupDescription =
          pReplicationSubnetGroupDescription_,
        tags = Lude.Nothing
      }

-- | One or more subnet IDs to be assigned to the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgSubnetIds :: Lens.Lens' CreateReplicationSubnetGroup [Lude.Text]
crsgSubnetIds = Lens.lens (subnetIds :: CreateReplicationSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateReplicationSubnetGroup)
{-# DEPRECATED crsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The name for the replication subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default".
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationSubnetGroup Lude.Text
crsgReplicationSubnetGroupIdentifier = Lens.lens (replicationSubnetGroupIdentifier :: CreateReplicationSubnetGroup -> Lude.Text) (\s a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationSubnetGroup)
{-# DEPRECATED crsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | The description for the subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgReplicationSubnetGroupDescription :: Lens.Lens' CreateReplicationSubnetGroup Lude.Text
crsgReplicationSubnetGroupDescription = Lens.lens (replicationSubnetGroupDescription :: CreateReplicationSubnetGroup -> Lude.Text) (\s a -> s {replicationSubnetGroupDescription = a} :: CreateReplicationSubnetGroup)
{-# DEPRECATED crsgReplicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead." #-}

-- | One or more tags to be assigned to the subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgTags :: Lens.Lens' CreateReplicationSubnetGroup (Lude.Maybe [Tag])
crsgTags = Lens.lens (tags :: CreateReplicationSubnetGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateReplicationSubnetGroup)
{-# DEPRECATED crsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateReplicationSubnetGroup where
  type
    Rs CreateReplicationSubnetGroup =
      CreateReplicationSubnetGroupResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReplicationSubnetGroupResponse'
            Lude.<$> (x Lude..?> "ReplicationSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReplicationSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.CreateReplicationSubnetGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReplicationSubnetGroup where
  toJSON CreateReplicationSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Lude..= replicationSubnetGroupIdentifier
              ),
            Lude.Just
              ( "ReplicationSubnetGroupDescription"
                  Lude..= replicationSubnetGroupDescription
              ),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateReplicationSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReplicationSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkCreateReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { -- | The replication subnet group that was created.
    replicationSubnetGroup :: Lude.Maybe ReplicationSubnetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'replicationSubnetGroup' - The replication subnet group that was created.
-- * 'responseStatus' - The response status code.
mkCreateReplicationSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReplicationSubnetGroupResponse
mkCreateReplicationSubnetGroupResponse pResponseStatus_ =
  CreateReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication subnet group that was created.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrsReplicationSubnetGroup :: Lens.Lens' CreateReplicationSubnetGroupResponse (Lude.Maybe ReplicationSubnetGroup)
crsgrsReplicationSubnetGroup = Lens.lens (replicationSubnetGroup :: CreateReplicationSubnetGroupResponse -> Lude.Maybe ReplicationSubnetGroup) (\s a -> s {replicationSubnetGroup = a} :: CreateReplicationSubnetGroupResponse)
{-# DEPRECATED crsgrsReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsgrsResponseStatus :: Lens.Lens' CreateReplicationSubnetGroupResponse Lude.Int
crsgrsResponseStatus = Lens.lens (responseStatus :: CreateReplicationSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReplicationSubnetGroupResponse)
{-# DEPRECATED crsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

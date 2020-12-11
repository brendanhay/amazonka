{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for the specified replication subnet group.
module Network.AWS.DMS.ModifyReplicationSubnetGroup
  ( -- * Creating a request
    ModifyReplicationSubnetGroup (..),
    mkModifyReplicationSubnetGroup,

    -- ** Request lenses
    mrsgReplicationSubnetGroupDescription,
    mrsgReplicationSubnetGroupIdentifier,
    mrsgSubnetIds,

    -- * Destructuring the response
    ModifyReplicationSubnetGroupResponse (..),
    mkModifyReplicationSubnetGroupResponse,

    -- ** Response lenses
    mrsgrsReplicationSubnetGroup,
    mrsgrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { replicationSubnetGroupDescription ::
      Lude.Maybe Lude.Text,
    replicationSubnetGroupIdentifier ::
      Lude.Text,
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

-- | Creates a value of 'ModifyReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- * 'replicationSubnetGroupDescription' - A description for the replication instance subnet group.
-- * 'replicationSubnetGroupIdentifier' - The name of the replication instance subnet group.
-- * 'subnetIds' - A list of subnet IDs.
mkModifyReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Lude.Text ->
  ModifyReplicationSubnetGroup
mkModifyReplicationSubnetGroup pReplicationSubnetGroupIdentifier_ =
  ModifyReplicationSubnetGroup'
    { replicationSubnetGroupDescription =
        Lude.Nothing,
      replicationSubnetGroupIdentifier =
        pReplicationSubnetGroupIdentifier_,
      subnetIds = Lude.mempty
    }

-- | A description for the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupDescription :: Lens.Lens' ModifyReplicationSubnetGroup (Lude.Maybe Lude.Text)
mrsgReplicationSubnetGroupDescription = Lens.lens (replicationSubnetGroupDescription :: ModifyReplicationSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {replicationSubnetGroupDescription = a} :: ModifyReplicationSubnetGroup)
{-# DEPRECATED mrsgReplicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead." #-}

-- | The name of the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgReplicationSubnetGroupIdentifier :: Lens.Lens' ModifyReplicationSubnetGroup Lude.Text
mrsgReplicationSubnetGroupIdentifier = Lens.lens (replicationSubnetGroupIdentifier :: ModifyReplicationSubnetGroup -> Lude.Text) (\s a -> s {replicationSubnetGroupIdentifier = a} :: ModifyReplicationSubnetGroup)
{-# DEPRECATED mrsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | A list of subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgSubnetIds :: Lens.Lens' ModifyReplicationSubnetGroup [Lude.Text]
mrsgSubnetIds = Lens.lens (subnetIds :: ModifyReplicationSubnetGroup -> [Lude.Text]) (\s a -> s {subnetIds = a} :: ModifyReplicationSubnetGroup)
{-# DEPRECATED mrsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.AWSRequest ModifyReplicationSubnetGroup where
  type
    Rs ModifyReplicationSubnetGroup =
      ModifyReplicationSubnetGroupResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyReplicationSubnetGroupResponse'
            Lude.<$> (x Lude..?> "ReplicationSubnetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReplicationSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.ModifyReplicationSubnetGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyReplicationSubnetGroup where
  toJSON ModifyReplicationSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReplicationSubnetGroupDescription" Lude..=)
              Lude.<$> replicationSubnetGroupDescription,
            Lude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Lude..= replicationSubnetGroupIdentifier
              ),
            Lude.Just ("SubnetIds" Lude..= subnetIds)
          ]
      )

instance Lude.ToPath ModifyReplicationSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReplicationSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkModifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { replicationSubnetGroup ::
      Lude.Maybe
        ReplicationSubnetGroup,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'replicationSubnetGroup' - The modified replication subnet group.
-- * 'responseStatus' - The response status code.
mkModifyReplicationSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReplicationSubnetGroupResponse
mkModifyReplicationSubnetGroupResponse pResponseStatus_ =
  ModifyReplicationSubnetGroupResponse'
    { replicationSubnetGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The modified replication subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrsReplicationSubnetGroup :: Lens.Lens' ModifyReplicationSubnetGroupResponse (Lude.Maybe ReplicationSubnetGroup)
mrsgrsReplicationSubnetGroup = Lens.lens (replicationSubnetGroup :: ModifyReplicationSubnetGroupResponse -> Lude.Maybe ReplicationSubnetGroup) (\s a -> s {replicationSubnetGroup = a} :: ModifyReplicationSubnetGroupResponse)
{-# DEPRECATED mrsgrsReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsgrsResponseStatus :: Lens.Lens' ModifyReplicationSubnetGroupResponse Lude.Int
mrsgrsResponseStatus = Lens.lens (responseStatus :: ModifyReplicationSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReplicationSubnetGroupResponse)
{-# DEPRECATED mrsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

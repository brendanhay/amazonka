{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DMS.DeleteReplicationSubnetGroup
  ( -- * Creating a request
    DeleteReplicationSubnetGroup (..),
    mkDeleteReplicationSubnetGroup,

    -- ** Request lenses
    drsgReplicationSubnetGroupIdentifier,

    -- * Destructuring the response
    DeleteReplicationSubnetGroupResponse (..),
    mkDeleteReplicationSubnetGroupResponse,

    -- ** Response lenses
    drsgrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteReplicationSubnetGroup' smart constructor.
newtype DeleteReplicationSubnetGroup = DeleteReplicationSubnetGroup'
  { replicationSubnetGroupIdentifier ::
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

-- | Creates a value of 'DeleteReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- * 'replicationSubnetGroupIdentifier' - The subnet group name of the replication instance.
mkDeleteReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Lude.Text ->
  DeleteReplicationSubnetGroup
mkDeleteReplicationSubnetGroup pReplicationSubnetGroupIdentifier_ =
  DeleteReplicationSubnetGroup'
    { replicationSubnetGroupIdentifier =
        pReplicationSubnetGroupIdentifier_
    }

-- | The subnet group name of the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgReplicationSubnetGroupIdentifier :: Lens.Lens' DeleteReplicationSubnetGroup Lude.Text
drsgReplicationSubnetGroupIdentifier = Lens.lens (replicationSubnetGroupIdentifier :: DeleteReplicationSubnetGroup -> Lude.Text) (\s a -> s {replicationSubnetGroupIdentifier = a} :: DeleteReplicationSubnetGroup)
{-# DEPRECATED drsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

instance Lude.AWSRequest DeleteReplicationSubnetGroup where
  type
    Rs DeleteReplicationSubnetGroup =
      DeleteReplicationSubnetGroupResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteReplicationSubnetGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationSubnetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DeleteReplicationSubnetGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReplicationSubnetGroup where
  toJSON DeleteReplicationSubnetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Lude..= replicationSubnetGroupIdentifier
              )
          ]
      )

instance Lude.ToPath DeleteReplicationSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationSubnetGroup where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteReplicationSubnetGroupResponse' smart constructor.
newtype DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReplicationSubnetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationSubnetGroupResponse
mkDeleteReplicationSubnetGroupResponse pResponseStatus_ =
  DeleteReplicationSubnetGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgrsResponseStatus :: Lens.Lens' DeleteReplicationSubnetGroupResponse Lude.Int
drsgrsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationSubnetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationSubnetGroupResponse)
{-# DEPRECATED drsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

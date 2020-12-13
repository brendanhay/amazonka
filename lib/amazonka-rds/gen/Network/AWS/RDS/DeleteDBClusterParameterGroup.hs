{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB cluster parameter group. The DB cluster parameter group to be deleted can't be associated with any DB clusters.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.DeleteDBClusterParameterGroup
  ( -- * Creating a request
    DeleteDBClusterParameterGroup (..),
    mkDeleteDBClusterParameterGroup,

    -- ** Request lenses
    ddcpgDBClusterParameterGroupName,

    -- * Destructuring the response
    DeleteDBClusterParameterGroupResponse (..),
    mkDeleteDBClusterParameterGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBClusterParameterGroup' smart constructor.
newtype DeleteDBClusterParameterGroup = DeleteDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    --     * Must be the name of an existing DB cluster parameter group.
    --
    --
    --     * You can't delete a default DB cluster parameter group.
    --
    --
    --     * Can't be associated with any DB clusters.
    dbClusterParameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB cluster parameter group.
--
--
--     * You can't delete a default DB cluster parameter group.
--
--
--     * Can't be associated with any DB clusters.
mkDeleteDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Lude.Text ->
  DeleteDBClusterParameterGroup
mkDeleteDBClusterParameterGroup pDBClusterParameterGroupName_ =
  DeleteDBClusterParameterGroup'
    { dbClusterParameterGroupName =
        pDBClusterParameterGroupName_
    }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB cluster parameter group.
--
--
--     * You can't delete a default DB cluster parameter group.
--
--
--     * Can't be associated with any DB clusters.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpgDBClusterParameterGroupName :: Lens.Lens' DeleteDBClusterParameterGroup Lude.Text
ddcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: DeleteDBClusterParameterGroup -> Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: DeleteDBClusterParameterGroup)
{-# DEPRECATED ddcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

instance Lude.AWSRequest DeleteDBClusterParameterGroup where
  type
    Rs DeleteDBClusterParameterGroup =
      DeleteDBClusterParameterGroupResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull DeleteDBClusterParameterGroupResponse'

instance Lude.ToHeaders DeleteDBClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBClusterParameterGroup where
  toQuery DeleteDBClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteDBClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName
      ]

-- | /See:/ 'mkDeleteDBClusterParameterGroupResponse' smart constructor.
data DeleteDBClusterParameterGroupResponse = DeleteDBClusterParameterGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBClusterParameterGroupResponse' with the minimum fields required to make a request.
mkDeleteDBClusterParameterGroupResponse ::
  DeleteDBClusterParameterGroupResponse
mkDeleteDBClusterParameterGroupResponse =
  DeleteDBClusterParameterGroupResponse'

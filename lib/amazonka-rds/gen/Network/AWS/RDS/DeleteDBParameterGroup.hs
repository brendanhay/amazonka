{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB parameter group. The DB parameter group to be deleted can't be associated with any DB instances.
module Network.AWS.RDS.DeleteDBParameterGroup
  ( -- * Creating a request
    DeleteDBParameterGroup (..),
    mkDeleteDBParameterGroup,

    -- ** Request lenses
    ddbpgDBParameterGroupName,

    -- * Destructuring the response
    DeleteDBParameterGroupResponse (..),
    mkDeleteDBParameterGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBParameterGroup' smart constructor.
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    --     * Must be the name of an existing DB parameter group
    --
    --
    --     * You can't delete a default DB parameter group
    --
    --
    --     * Can't be associated with any DB instances
    dbParameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB parameter group
--
--
--     * You can't delete a default DB parameter group
--
--
--     * Can't be associated with any DB instances
mkDeleteDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Lude.Text ->
  DeleteDBParameterGroup
mkDeleteDBParameterGroup pDBParameterGroupName_ =
  DeleteDBParameterGroup'
    { dbParameterGroupName =
        pDBParameterGroupName_
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * Must be the name of an existing DB parameter group
--
--
--     * You can't delete a default DB parameter group
--
--
--     * Can't be associated with any DB instances
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgDBParameterGroupName :: Lens.Lens' DeleteDBParameterGroup Lude.Text
ddbpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: DeleteDBParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DeleteDBParameterGroup)
{-# DEPRECATED ddbpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

instance Lude.AWSRequest DeleteDBParameterGroup where
  type Rs DeleteDBParameterGroup = DeleteDBParameterGroupResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull DeleteDBParameterGroupResponse'

instance Lude.ToHeaders DeleteDBParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBParameterGroup where
  toQuery DeleteDBParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBParameterGroupName" Lude.=: dbParameterGroupName
      ]

-- | /See:/ 'mkDeleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBParameterGroupResponse' with the minimum fields required to make a request.
mkDeleteDBParameterGroupResponse ::
  DeleteDBParameterGroupResponse
mkDeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'

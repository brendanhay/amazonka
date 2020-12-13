{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
module Network.AWS.RDS.DeleteDBSecurityGroup
  ( -- * Creating a request
    DeleteDBSecurityGroup (..),
    mkDeleteDBSecurityGroup,

    -- ** Request lenses
    ddsgDBSecurityGroupName,

    -- * Destructuring the response
    DeleteDBSecurityGroupResponse (..),
    mkDeleteDBSecurityGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBSecurityGroup' smart constructor.
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup'
  { -- | The name of the DB security group to delete.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    --     * Must not be "Default"
    dbSecurityGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBSecurityGroup' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroupName' - The name of the DB security group to delete.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--     * Must not be "Default"
mkDeleteDBSecurityGroup ::
  -- | 'dbSecurityGroupName'
  Lude.Text ->
  DeleteDBSecurityGroup
mkDeleteDBSecurityGroup pDBSecurityGroupName_ =
  DeleteDBSecurityGroup'
    { dbSecurityGroupName =
        pDBSecurityGroupName_
    }

-- | The name of the DB security group to delete.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--     * Must not be "Default"
--
--
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsgDBSecurityGroupName :: Lens.Lens' DeleteDBSecurityGroup Lude.Text
ddsgDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: DeleteDBSecurityGroup -> Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: DeleteDBSecurityGroup)
{-# DEPRECATED ddsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

instance Lude.AWSRequest DeleteDBSecurityGroup where
  type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull DeleteDBSecurityGroupResponse'

instance Lude.ToHeaders DeleteDBSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBSecurityGroup where
  toQuery DeleteDBSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSecurityGroupName" Lude.=: dbSecurityGroupName
      ]

-- | /See:/ 'mkDeleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBSecurityGroupResponse' with the minimum fields required to make a request.
mkDeleteDBSecurityGroupResponse ::
  DeleteDBSecurityGroupResponse
mkDeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'

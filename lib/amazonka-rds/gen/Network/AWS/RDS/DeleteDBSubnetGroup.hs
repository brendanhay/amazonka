{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB subnet group.
module Network.AWS.RDS.DeleteDBSubnetGroup
  ( -- * Creating a request
    DeleteDBSubnetGroup (..),
    mkDeleteDBSubnetGroup,

    -- ** Request lenses
    ddbsgDBSubnetGroupName,

    -- * Destructuring the response
    DeleteDBSubnetGroupResponse (..),
    mkDeleteDBSubnetGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBSubnetGroup' smart constructor.
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'
  { dbSubnetGroupName ::
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

-- | Creates a value of 'DeleteDBSubnetGroup' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroupName' - The name of the database subnet group to delete.
--
-- Constraints:
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
mkDeleteDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Lude.Text ->
  DeleteDBSubnetGroup
mkDeleteDBSubnetGroup pDBSubnetGroupName_ =
  DeleteDBSubnetGroup' {dbSubnetGroupName = pDBSubnetGroupName_}

-- | The name of the database subnet group to delete.
--
-- Constraints:
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgDBSubnetGroupName :: Lens.Lens' DeleteDBSubnetGroup Lude.Text
ddbsgDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: DeleteDBSubnetGroup -> Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: DeleteDBSubnetGroup)
{-# DEPRECATED ddbsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

instance Lude.AWSRequest DeleteDBSubnetGroup where
  type Rs DeleteDBSubnetGroup = DeleteDBSubnetGroupResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull DeleteDBSubnetGroupResponse'

instance Lude.ToHeaders DeleteDBSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBSubnetGroup where
  toQuery DeleteDBSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName
      ]

-- | /See:/ 'mkDeleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBSubnetGroupResponse' with the minimum fields required to make a request.
mkDeleteDBSubnetGroupResponse ::
  DeleteDBSubnetGroupResponse
mkDeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'

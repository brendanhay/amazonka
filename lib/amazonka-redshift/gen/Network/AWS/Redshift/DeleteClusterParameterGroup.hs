{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified Amazon Redshift parameter group.
module Network.AWS.Redshift.DeleteClusterParameterGroup
  ( -- * Creating a request
    DeleteClusterParameterGroup (..),
    mkDeleteClusterParameterGroup,

    -- ** Request lenses
    dParameterGroupName,

    -- * Destructuring the response
    DeleteClusterParameterGroupResponse (..),
    mkDeleteClusterParameterGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteClusterParameterGroup' smart constructor.
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
  { parameterGroupName ::
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

-- | Creates a value of 'DeleteClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'parameterGroupName' - The name of the parameter group to be deleted.
--
-- Constraints:
--
--     * Must be the name of an existing cluster parameter group.
--
--
--     * Cannot delete a default cluster parameter group.
mkDeleteClusterParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  DeleteClusterParameterGroup
mkDeleteClusterParameterGroup pParameterGroupName_ =
  DeleteClusterParameterGroup'
    { parameterGroupName =
        pParameterGroupName_
    }

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
--     * Must be the name of an existing cluster parameter group.
--
--
--     * Cannot delete a default cluster parameter group.
--
--
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameterGroupName :: Lens.Lens' DeleteClusterParameterGroup Lude.Text
dParameterGroupName = Lens.lens (parameterGroupName :: DeleteClusterParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: DeleteClusterParameterGroup)
{-# DEPRECATED dParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest DeleteClusterParameterGroup where
  type
    Rs DeleteClusterParameterGroup =
      DeleteClusterParameterGroupResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteClusterParameterGroupResponse'

instance Lude.ToHeaders DeleteClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClusterParameterGroup where
  toQuery DeleteClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ParameterGroupName" Lude.=: parameterGroupName
      ]

-- | /See:/ 'mkDeleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterParameterGroupResponse' with the minimum fields required to make a request.
mkDeleteClusterParameterGroupResponse ::
  DeleteClusterParameterGroupResponse
mkDeleteClusterParameterGroupResponse =
  DeleteClusterParameterGroupResponse'

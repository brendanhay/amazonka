{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified placement group. You must terminate all instances
-- in the placement group before you can delete the placement group. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.DeletePlacementGroup
  ( -- * Creating a Request
    DeletePlacementGroup (..),
    newDeletePlacementGroup,

    -- * Request Lenses
    deletePlacementGroup_dryRun,
    deletePlacementGroup_groupName,

    -- * Destructuring the Response
    DeletePlacementGroupResponse (..),
    newDeletePlacementGroupResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePlacementGroup' smart constructor.
data DeletePlacementGroup = DeletePlacementGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the placement group.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePlacementGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deletePlacementGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupName', 'deletePlacementGroup_groupName' - The name of the placement group.
newDeletePlacementGroup ::
  -- | 'groupName'
  Prelude.Text ->
  DeletePlacementGroup
newDeletePlacementGroup pGroupName_ =
  DeletePlacementGroup'
    { dryRun = Prelude.Nothing,
      groupName = pGroupName_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deletePlacementGroup_dryRun :: Lens.Lens' DeletePlacementGroup (Prelude.Maybe Prelude.Bool)
deletePlacementGroup_dryRun = Lens.lens (\DeletePlacementGroup' {dryRun} -> dryRun) (\s@DeletePlacementGroup' {} a -> s {dryRun = a} :: DeletePlacementGroup)

-- | The name of the placement group.
deletePlacementGroup_groupName :: Lens.Lens' DeletePlacementGroup Prelude.Text
deletePlacementGroup_groupName = Lens.lens (\DeletePlacementGroup' {groupName} -> groupName) (\s@DeletePlacementGroup' {} a -> s {groupName = a} :: DeletePlacementGroup)

instance Prelude.AWSRequest DeletePlacementGroup where
  type
    Rs DeletePlacementGroup =
      DeletePlacementGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeletePlacementGroupResponse'

instance Prelude.Hashable DeletePlacementGroup

instance Prelude.NFData DeletePlacementGroup

instance Prelude.ToHeaders DeletePlacementGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePlacementGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePlacementGroup where
  toQuery DeletePlacementGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeletePlacementGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "GroupName" Prelude.=: groupName
      ]

-- | /See:/ 'newDeletePlacementGroupResponse' smart constructor.
data DeletePlacementGroupResponse = DeletePlacementGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePlacementGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePlacementGroupResponse ::
  DeletePlacementGroupResponse
newDeletePlacementGroupResponse =
  DeletePlacementGroupResponse'

instance Prelude.NFData DeletePlacementGroupResponse

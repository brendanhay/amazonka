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
-- Module      : Amazonka.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified placement group. You must terminate all instances
-- in the placement group before you can delete the placement group. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DeletePlacementGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeletePlacementGroup where
  type
    AWSResponse DeletePlacementGroup =
      DeletePlacementGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeletePlacementGroupResponse'

instance Prelude.Hashable DeletePlacementGroup where
  hashWithSalt _salt DeletePlacementGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData DeletePlacementGroup where
  rnf DeletePlacementGroup' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf groupName

instance Data.ToHeaders DeletePlacementGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePlacementGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePlacementGroup where
  toQuery DeletePlacementGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeletePlacementGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GroupName" Data.=: groupName
      ]

-- | /See:/ 'newDeletePlacementGroupResponse' smart constructor.
data DeletePlacementGroupResponse = DeletePlacementGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlacementGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePlacementGroupResponse ::
  DeletePlacementGroupResponse
newDeletePlacementGroupResponse =
  DeletePlacementGroupResponse'

instance Prelude.NFData DeletePlacementGroupResponse where
  rnf _ = ()

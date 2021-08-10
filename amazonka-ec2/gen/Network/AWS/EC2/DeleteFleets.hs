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
-- Module      : Network.AWS.EC2.DeleteFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EC2 Fleet.
--
-- After you delete an EC2 Fleet, it launches no new instances.
--
-- You must specify whether a deleted EC2 Fleet should also terminate its
-- instances. If you choose to terminate the instances, the EC2 Fleet
-- enters the @deleted_terminating@ state. Otherwise, the EC2 Fleet enters
-- the @deleted_running@ state, and the instances continue to run until
-- they are interrupted or you terminate them manually.
--
-- For @instant@ fleets, EC2 Fleet must terminate the instances when the
-- fleet is deleted. A deleted @instant@ fleet with running instances is
-- not supported.
--
-- __Restrictions__
--
-- -   You can delete up to 25 @instant@ fleets in a single request. If you
--     exceed this number, no @instant@ fleets are deleted and an error is
--     returned. There is no restriction on the number of fleets of type
--     @maintain@ or @request@ that can be deleted in a single request.
--
-- -   Up to 1000 instances can be terminated in a single request to delete
--     @instant@ fleets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#delete-fleet Deleting an EC2 Fleet>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.DeleteFleets
  ( -- * Creating a Request
    DeleteFleets (..),
    newDeleteFleets,

    -- * Request Lenses
    deleteFleets_dryRun,
    deleteFleets_fleetIds,
    deleteFleets_terminateInstances,

    -- * Destructuring the Response
    DeleteFleetsResponse (..),
    newDeleteFleetsResponse,

    -- * Response Lenses
    deleteFleetsResponse_unsuccessfulFleetDeletions,
    deleteFleetsResponse_successfulFleetDeletions,
    deleteFleetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFleets' smart constructor.
data DeleteFleets = DeleteFleets'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the EC2 Fleets.
    fleetIds :: [Prelude.Text],
    -- | Indicates whether to terminate the instances when the EC2 Fleet is
    -- deleted. The default is to terminate the instances.
    --
    -- To let the instances continue to run after the EC2 Fleet is deleted,
    -- specify @NoTerminateInstances@. Supported only for fleets of type
    -- @maintain@ and @request@.
    --
    -- For @instant@ fleets, you cannot specify @NoTerminateInstances@. A
    -- deleted @instant@ fleet with running instances is not supported.
    terminateInstances :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteFleets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'fleetIds', 'deleteFleets_fleetIds' - The IDs of the EC2 Fleets.
--
-- 'terminateInstances', 'deleteFleets_terminateInstances' - Indicates whether to terminate the instances when the EC2 Fleet is
-- deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted,
-- specify @NoTerminateInstances@. Supported only for fleets of type
-- @maintain@ and @request@.
--
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@. A
-- deleted @instant@ fleet with running instances is not supported.
newDeleteFleets ::
  -- | 'terminateInstances'
  Prelude.Bool ->
  DeleteFleets
newDeleteFleets pTerminateInstances_ =
  DeleteFleets'
    { dryRun = Prelude.Nothing,
      fleetIds = Prelude.mempty,
      terminateInstances = pTerminateInstances_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteFleets_dryRun :: Lens.Lens' DeleteFleets (Prelude.Maybe Prelude.Bool)
deleteFleets_dryRun = Lens.lens (\DeleteFleets' {dryRun} -> dryRun) (\s@DeleteFleets' {} a -> s {dryRun = a} :: DeleteFleets)

-- | The IDs of the EC2 Fleets.
deleteFleets_fleetIds :: Lens.Lens' DeleteFleets [Prelude.Text]
deleteFleets_fleetIds = Lens.lens (\DeleteFleets' {fleetIds} -> fleetIds) (\s@DeleteFleets' {} a -> s {fleetIds = a} :: DeleteFleets) Prelude.. Lens._Coerce

-- | Indicates whether to terminate the instances when the EC2 Fleet is
-- deleted. The default is to terminate the instances.
--
-- To let the instances continue to run after the EC2 Fleet is deleted,
-- specify @NoTerminateInstances@. Supported only for fleets of type
-- @maintain@ and @request@.
--
-- For @instant@ fleets, you cannot specify @NoTerminateInstances@. A
-- deleted @instant@ fleet with running instances is not supported.
deleteFleets_terminateInstances :: Lens.Lens' DeleteFleets Prelude.Bool
deleteFleets_terminateInstances = Lens.lens (\DeleteFleets' {terminateInstances} -> terminateInstances) (\s@DeleteFleets' {} a -> s {terminateInstances = a} :: DeleteFleets)

instance Core.AWSRequest DeleteFleets where
  type AWSResponse DeleteFleets = DeleteFleetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteFleetsResponse'
            Prelude.<$> ( x Core..@? "unsuccessfulFleetDeletionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "successfulFleetDeletionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFleets

instance Prelude.NFData DeleteFleets

instance Core.ToHeaders DeleteFleets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteFleets where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFleets where
  toQuery DeleteFleets' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteFleets" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "FleetId" fleetIds,
        "TerminateInstances" Core.=: terminateInstances
      ]

-- | /See:/ 'newDeleteFleetsResponse' smart constructor.
data DeleteFleetsResponse = DeleteFleetsResponse'
  { -- | Information about the EC2 Fleets that are not successfully deleted.
    unsuccessfulFleetDeletions :: Prelude.Maybe [DeleteFleetErrorItem],
    -- | Information about the EC2 Fleets that are successfully deleted.
    successfulFleetDeletions :: Prelude.Maybe [DeleteFleetSuccessItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessfulFleetDeletions', 'deleteFleetsResponse_unsuccessfulFleetDeletions' - Information about the EC2 Fleets that are not successfully deleted.
--
-- 'successfulFleetDeletions', 'deleteFleetsResponse_successfulFleetDeletions' - Information about the EC2 Fleets that are successfully deleted.
--
-- 'httpStatus', 'deleteFleetsResponse_httpStatus' - The response's http status code.
newDeleteFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFleetsResponse
newDeleteFleetsResponse pHttpStatus_ =
  DeleteFleetsResponse'
    { unsuccessfulFleetDeletions =
        Prelude.Nothing,
      successfulFleetDeletions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the EC2 Fleets that are not successfully deleted.
deleteFleetsResponse_unsuccessfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Prelude.Maybe [DeleteFleetErrorItem])
deleteFleetsResponse_unsuccessfulFleetDeletions = Lens.lens (\DeleteFleetsResponse' {unsuccessfulFleetDeletions} -> unsuccessfulFleetDeletions) (\s@DeleteFleetsResponse' {} a -> s {unsuccessfulFleetDeletions = a} :: DeleteFleetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the EC2 Fleets that are successfully deleted.
deleteFleetsResponse_successfulFleetDeletions :: Lens.Lens' DeleteFleetsResponse (Prelude.Maybe [DeleteFleetSuccessItem])
deleteFleetsResponse_successfulFleetDeletions = Lens.lens (\DeleteFleetsResponse' {successfulFleetDeletions} -> successfulFleetDeletions) (\s@DeleteFleetsResponse' {} a -> s {successfulFleetDeletions = a} :: DeleteFleetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteFleetsResponse_httpStatus :: Lens.Lens' DeleteFleetsResponse Prelude.Int
deleteFleetsResponse_httpStatus = Lens.lens (\DeleteFleetsResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetsResponse' {} a -> s {httpStatus = a} :: DeleteFleetsResponse)

instance Prelude.NFData DeleteFleetsResponse

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
-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data feed for Spot Instances.
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
  ( -- * Creating a Request
    DeleteSpotDatafeedSubscription (..),
    newDeleteSpotDatafeedSubscription,

    -- * Request Lenses
    deleteSpotDatafeedSubscription_dryRun,

    -- * Destructuring the Response
    DeleteSpotDatafeedSubscriptionResponse (..),
    newDeleteSpotDatafeedSubscriptionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
-- /See:/ 'newDeleteSpotDatafeedSubscription' smart constructor.
data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSpotDatafeedSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteSpotDatafeedSubscription_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDeleteSpotDatafeedSubscription ::
  DeleteSpotDatafeedSubscription
newDeleteSpotDatafeedSubscription =
  DeleteSpotDatafeedSubscription'
    { dryRun =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSpotDatafeedSubscription_dryRun :: Lens.Lens' DeleteSpotDatafeedSubscription (Core.Maybe Core.Bool)
deleteSpotDatafeedSubscription_dryRun = Lens.lens (\DeleteSpotDatafeedSubscription' {dryRun} -> dryRun) (\s@DeleteSpotDatafeedSubscription' {} a -> s {dryRun = a} :: DeleteSpotDatafeedSubscription)

instance
  Core.AWSRequest
    DeleteSpotDatafeedSubscription
  where
  type
    AWSResponse DeleteSpotDatafeedSubscription =
      DeleteSpotDatafeedSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteSpotDatafeedSubscriptionResponse'

instance Core.Hashable DeleteSpotDatafeedSubscription

instance Core.NFData DeleteSpotDatafeedSubscription

instance
  Core.ToHeaders
    DeleteSpotDatafeedSubscription
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSpotDatafeedSubscription where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSpotDatafeedSubscription where
  toQuery DeleteSpotDatafeedSubscription' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteSpotDatafeedSubscription" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSpotDatafeedSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSpotDatafeedSubscriptionResponse ::
  DeleteSpotDatafeedSubscriptionResponse
newDeleteSpotDatafeedSubscriptionResponse =
  DeleteSpotDatafeedSubscriptionResponse'

instance
  Core.NFData
    DeleteSpotDatafeedSubscriptionResponse

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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSpotDatafeedSubscription_dryRun :: Lens.Lens' DeleteSpotDatafeedSubscription (Prelude.Maybe Prelude.Bool)
deleteSpotDatafeedSubscription_dryRun = Lens.lens (\DeleteSpotDatafeedSubscription' {dryRun} -> dryRun) (\s@DeleteSpotDatafeedSubscription' {} a -> s {dryRun = a} :: DeleteSpotDatafeedSubscription)

instance
  Prelude.AWSRequest
    DeleteSpotDatafeedSubscription
  where
  type
    Rs DeleteSpotDatafeedSubscription =
      DeleteSpotDatafeedSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteSpotDatafeedSubscriptionResponse'

instance
  Prelude.Hashable
    DeleteSpotDatafeedSubscription

instance
  Prelude.NFData
    DeleteSpotDatafeedSubscription

instance
  Prelude.ToHeaders
    DeleteSpotDatafeedSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteSpotDatafeedSubscription
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteSpotDatafeedSubscription
  where
  toQuery DeleteSpotDatafeedSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteSpotDatafeedSubscription" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSpotDatafeedSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSpotDatafeedSubscriptionResponse ::
  DeleteSpotDatafeedSubscriptionResponse
newDeleteSpotDatafeedSubscriptionResponse =
  DeleteSpotDatafeedSubscriptionResponse'

instance
  Prelude.NFData
    DeleteSpotDatafeedSubscriptionResponse

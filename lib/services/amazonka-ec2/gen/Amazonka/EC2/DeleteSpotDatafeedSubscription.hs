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
-- Module      : Amazonka.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data feed for Spot Instances.
module Amazonka.EC2.DeleteSpotDatafeedSubscription
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteSpotDatafeedSubscription
  where
  type
    AWSResponse DeleteSpotDatafeedSubscription =
      DeleteSpotDatafeedSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSpotDatafeedSubscriptionResponse'

instance
  Prelude.Hashable
    DeleteSpotDatafeedSubscription
  where
  hashWithSalt
    _salt
    DeleteSpotDatafeedSubscription' {..} =
      _salt `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    DeleteSpotDatafeedSubscription
  where
  rnf DeleteSpotDatafeedSubscription' {..} =
    Prelude.rnf dryRun

instance
  Core.ToHeaders
    DeleteSpotDatafeedSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteSpotDatafeedSubscription where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteSpotDatafeedSubscription where
  toQuery DeleteSpotDatafeedSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteSpotDatafeedSubscription" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()

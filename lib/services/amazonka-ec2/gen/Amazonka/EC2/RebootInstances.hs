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
-- Module      : Amazonka.EC2.RebootInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a reboot of the specified instances. This operation is
-- asynchronous; it only queues a request to reboot the specified
-- instances. The operation succeeds if the instances are valid and belong
-- to you. Requests to reboot terminated instances are ignored.
--
-- If an instance does not cleanly shut down within a few minutes, Amazon
-- EC2 performs a hard reboot.
--
-- For more information about troubleshooting, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html Troubleshoot an unreachable instance>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.RebootInstances
  ( -- * Creating a Request
    RebootInstances (..),
    newRebootInstances,

    -- * Request Lenses
    rebootInstances_dryRun,
    rebootInstances_instanceIds,

    -- * Destructuring the Response
    RebootInstancesResponse (..),
    newRebootInstancesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebootInstances' smart constructor.
data RebootInstances = RebootInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance IDs.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rebootInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceIds', 'rebootInstances_instanceIds' - The instance IDs.
newRebootInstances ::
  RebootInstances
newRebootInstances =
  RebootInstances'
    { dryRun = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rebootInstances_dryRun :: Lens.Lens' RebootInstances (Prelude.Maybe Prelude.Bool)
rebootInstances_dryRun = Lens.lens (\RebootInstances' {dryRun} -> dryRun) (\s@RebootInstances' {} a -> s {dryRun = a} :: RebootInstances)

-- | The instance IDs.
rebootInstances_instanceIds :: Lens.Lens' RebootInstances [Prelude.Text]
rebootInstances_instanceIds = Lens.lens (\RebootInstances' {instanceIds} -> instanceIds) (\s@RebootInstances' {} a -> s {instanceIds = a} :: RebootInstances) Prelude.. Lens.coerced

instance Core.AWSRequest RebootInstances where
  type
    AWSResponse RebootInstances =
      RebootInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull RebootInstancesResponse'

instance Prelude.Hashable RebootInstances where
  hashWithSalt _salt RebootInstances' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData RebootInstances where
  rnf RebootInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceIds

instance Data.ToHeaders RebootInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RebootInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootInstances where
  toQuery RebootInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RebootInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newRebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebootInstancesResponse ::
  RebootInstancesResponse
newRebootInstancesResponse = RebootInstancesResponse'

instance Prelude.NFData RebootInstancesResponse where
  rnf _ = ()

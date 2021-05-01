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
-- Module      : Network.AWS.EC2.RebootInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html Getting console output and rebooting instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.RebootInstances
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
rebootInstances_instanceIds = Lens.lens (\RebootInstances' {instanceIds} -> instanceIds) (\s@RebootInstances' {} a -> s {instanceIds = a} :: RebootInstances) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RebootInstances where
  type Rs RebootInstances = RebootInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull RebootInstancesResponse'

instance Prelude.Hashable RebootInstances

instance Prelude.NFData RebootInstances

instance Prelude.ToHeaders RebootInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RebootInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RebootInstances where
  toQuery RebootInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RebootInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newRebootInstancesResponse' smart constructor.
data RebootInstancesResponse = RebootInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebootInstancesResponse ::
  RebootInstancesResponse
newRebootInstancesResponse = RebootInstancesResponse'

instance Prelude.NFData RebootInstancesResponse

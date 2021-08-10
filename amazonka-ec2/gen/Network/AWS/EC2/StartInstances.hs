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
-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon EBS-backed instance that you\'ve previously stopped.
--
-- Instances that use Amazon EBS volumes as their root devices can be
-- quickly stopped and started. When an instance is stopped, the compute
-- resources are released and you are not billed for instance usage.
-- However, your root partition Amazon EBS volume remains and continues to
-- persist your data, and you are charged for Amazon EBS volume usage. You
-- can restart your instance at any time. Every time you start your Windows
-- instance, Amazon EC2 charges you for a full instance hour. If you stop
-- and restart your Windows instance, a new instance hour begins and Amazon
-- EC2 charges you for another full instance hour even if you are still
-- within the same 60-minute period when it was stopped. Every time you
-- start your Linux instance, Amazon EC2 charges a one-minute minimum for
-- instance usage, and thereafter charges per second for instance usage.
--
-- Before stopping an instance, make sure it is in a state from which it
-- can be restarted. Stopping an instance does not preserve data stored in
-- RAM.
--
-- Performing this operation on an instance that uses an instance store as
-- its root device returns an error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stopping instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.StartInstances
  ( -- * Creating a Request
    StartInstances (..),
    newStartInstances,

    -- * Request Lenses
    startInstances_additionalInfo,
    startInstances_dryRun,
    startInstances_instanceIds,

    -- * Destructuring the Response
    StartInstancesResponse (..),
    newStartInstancesResponse,

    -- * Response Lenses
    startInstancesResponse_startingInstances,
    startInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartInstances' smart constructor.
data StartInstances = StartInstances'
  { -- | Reserved.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the instances.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'startInstances_additionalInfo' - Reserved.
--
-- 'dryRun', 'startInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceIds', 'startInstances_instanceIds' - The IDs of the instances.
newStartInstances ::
  StartInstances
newStartInstances =
  StartInstances'
    { additionalInfo = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Reserved.
startInstances_additionalInfo :: Lens.Lens' StartInstances (Prelude.Maybe Prelude.Text)
startInstances_additionalInfo = Lens.lens (\StartInstances' {additionalInfo} -> additionalInfo) (\s@StartInstances' {} a -> s {additionalInfo = a} :: StartInstances)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
startInstances_dryRun :: Lens.Lens' StartInstances (Prelude.Maybe Prelude.Bool)
startInstances_dryRun = Lens.lens (\StartInstances' {dryRun} -> dryRun) (\s@StartInstances' {} a -> s {dryRun = a} :: StartInstances)

-- | The IDs of the instances.
startInstances_instanceIds :: Lens.Lens' StartInstances [Prelude.Text]
startInstances_instanceIds = Lens.lens (\StartInstances' {instanceIds} -> instanceIds) (\s@StartInstances' {} a -> s {instanceIds = a} :: StartInstances) Prelude.. Lens._Coerce

instance Core.AWSRequest StartInstances where
  type
    AWSResponse StartInstances =
      StartInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          StartInstancesResponse'
            Prelude.<$> ( x Core..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstances

instance Prelude.NFData StartInstances

instance Core.ToHeaders StartInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath StartInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery StartInstances where
  toQuery StartInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("StartInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "AdditionalInfo" Core.=: additionalInfo,
        "DryRun" Core.=: dryRun,
        Core.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newStartInstancesResponse' smart constructor.
data StartInstancesResponse = StartInstancesResponse'
  { -- | Information about the started instances.
    startingInstances :: Prelude.Maybe [InstanceStateChange],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startingInstances', 'startInstancesResponse_startingInstances' - Information about the started instances.
--
-- 'httpStatus', 'startInstancesResponse_httpStatus' - The response's http status code.
newStartInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInstancesResponse
newStartInstancesResponse pHttpStatus_ =
  StartInstancesResponse'
    { startingInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the started instances.
startInstancesResponse_startingInstances :: Lens.Lens' StartInstancesResponse (Prelude.Maybe [InstanceStateChange])
startInstancesResponse_startingInstances = Lens.lens (\StartInstancesResponse' {startingInstances} -> startingInstances) (\s@StartInstancesResponse' {} a -> s {startingInstances = a} :: StartInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
startInstancesResponse_httpStatus :: Lens.Lens' StartInstancesResponse Prelude.Int
startInstancesResponse_httpStatus = Lens.lens (\StartInstancesResponse' {httpStatus} -> httpStatus) (\s@StartInstancesResponse' {} a -> s {httpStatus = a} :: StartInstancesResponse)

instance Prelude.NFData StartInstancesResponse

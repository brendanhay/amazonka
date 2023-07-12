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
-- Module      : Amazonka.EC2.StartInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- can restart your instance at any time. Every time you start your
-- instance, Amazon EC2 charges a one-minute minimum for instance usage,
-- and thereafter charges per second for instance usage.
--
-- Before stopping an instance, make sure it is in a state from which it
-- can be restarted. Stopping an instance does not preserve data stored in
-- RAM.
--
-- Performing this operation on an instance that uses an instance store as
-- its root device returns an error.
--
-- If you attempt to start a T3 instance with @host@ tenancy and the
-- @unlimted@ CPU credit option, the request fails. The @unlimited@ CPU
-- credit option is not supported on Dedicated Hosts. Before you start the
-- instance, either change its CPU credit option to @standard@, or change
-- its tenancy to @default@ or @dedicated@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html Stop and start your instance>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.StartInstances
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
startInstances_instanceIds = Lens.lens (\StartInstances' {instanceIds} -> instanceIds) (\s@StartInstances' {} a -> s {instanceIds = a} :: StartInstances) Prelude.. Lens.coerced

instance Core.AWSRequest StartInstances where
  type
    AWSResponse StartInstances =
      StartInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          StartInstancesResponse'
            Prelude.<$> ( x
                            Data..@? "instancesSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstances where
  hashWithSalt _salt StartInstances' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData StartInstances where
  rnf StartInstances' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceIds

instance Data.ToHeaders StartInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery StartInstances where
  toQuery StartInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AdditionalInfo" Data.=: additionalInfo,
        "DryRun" Data.=: dryRun,
        Data.toQueryList "InstanceId" instanceIds
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
startInstancesResponse_startingInstances = Lens.lens (\StartInstancesResponse' {startingInstances} -> startingInstances) (\s@StartInstancesResponse' {} a -> s {startingInstances = a} :: StartInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startInstancesResponse_httpStatus :: Lens.Lens' StartInstancesResponse Prelude.Int
startInstancesResponse_httpStatus = Lens.lens (\StartInstancesResponse' {httpStatus} -> httpStatus) (\s@StartInstancesResponse' {} a -> s {httpStatus = a} :: StartInstancesResponse)

instance Prelude.NFData StartInstancesResponse where
  rnf StartInstancesResponse' {..} =
    Prelude.rnf startingInstances
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.CloudHSMv2.CreateHsm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new hardware security module (HSM) in the specified AWS
-- CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateHsm
  ( -- * Creating a Request
    CreateHsm (..),
    newCreateHsm,

    -- * Request Lenses
    createHsm_ipAddress,
    createHsm_clusterId,
    createHsm_availabilityZone,

    -- * Destructuring the Response
    CreateHsmResponse (..),
    newCreateHsmResponse,

    -- * Response Lenses
    createHsmResponse_hsm,
    createHsmResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateHsm' smart constructor.
data CreateHsm = CreateHsm'
  { -- | The HSM\'s IP address. If you specify an IP address, use an available
    -- address from the subnet that maps to the Availability Zone where you are
    -- creating the HSM. If you don\'t specify an IP address, one is chosen for
    -- you from that subnet.
    ipAddress :: Core.Maybe Core.Text,
    -- | The identifier (ID) of the HSM\'s cluster. To find the cluster ID, use
    -- DescribeClusters.
    clusterId :: Core.Text,
    -- | The Availability Zone where you are creating the HSM. To find the
    -- cluster\'s Availability Zones, use DescribeClusters.
    availabilityZone :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'createHsm_ipAddress' - The HSM\'s IP address. If you specify an IP address, use an available
-- address from the subnet that maps to the Availability Zone where you are
-- creating the HSM. If you don\'t specify an IP address, one is chosen for
-- you from that subnet.
--
-- 'clusterId', 'createHsm_clusterId' - The identifier (ID) of the HSM\'s cluster. To find the cluster ID, use
-- DescribeClusters.
--
-- 'availabilityZone', 'createHsm_availabilityZone' - The Availability Zone where you are creating the HSM. To find the
-- cluster\'s Availability Zones, use DescribeClusters.
newCreateHsm ::
  -- | 'clusterId'
  Core.Text ->
  -- | 'availabilityZone'
  Core.Text ->
  CreateHsm
newCreateHsm pClusterId_ pAvailabilityZone_ =
  CreateHsm'
    { ipAddress = Core.Nothing,
      clusterId = pClusterId_,
      availabilityZone = pAvailabilityZone_
    }

-- | The HSM\'s IP address. If you specify an IP address, use an available
-- address from the subnet that maps to the Availability Zone where you are
-- creating the HSM. If you don\'t specify an IP address, one is chosen for
-- you from that subnet.
createHsm_ipAddress :: Lens.Lens' CreateHsm (Core.Maybe Core.Text)
createHsm_ipAddress = Lens.lens (\CreateHsm' {ipAddress} -> ipAddress) (\s@CreateHsm' {} a -> s {ipAddress = a} :: CreateHsm)

-- | The identifier (ID) of the HSM\'s cluster. To find the cluster ID, use
-- DescribeClusters.
createHsm_clusterId :: Lens.Lens' CreateHsm Core.Text
createHsm_clusterId = Lens.lens (\CreateHsm' {clusterId} -> clusterId) (\s@CreateHsm' {} a -> s {clusterId = a} :: CreateHsm)

-- | The Availability Zone where you are creating the HSM. To find the
-- cluster\'s Availability Zones, use DescribeClusters.
createHsm_availabilityZone :: Lens.Lens' CreateHsm Core.Text
createHsm_availabilityZone = Lens.lens (\CreateHsm' {availabilityZone} -> availabilityZone) (\s@CreateHsm' {} a -> s {availabilityZone = a} :: CreateHsm)

instance Core.AWSRequest CreateHsm where
  type AWSResponse CreateHsm = CreateHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHsmResponse'
            Core.<$> (x Core..?> "Hsm")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateHsm

instance Core.NFData CreateHsm

instance Core.ToHeaders CreateHsm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("BaldrApiService.CreateHsm" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateHsm where
  toJSON CreateHsm' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IpAddress" Core..=) Core.<$> ipAddress,
            Core.Just ("ClusterId" Core..= clusterId),
            Core.Just
              ("AvailabilityZone" Core..= availabilityZone)
          ]
      )

instance Core.ToPath CreateHsm where
  toPath = Core.const "/"

instance Core.ToQuery CreateHsm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateHsmResponse' smart constructor.
data CreateHsmResponse = CreateHsmResponse'
  { -- | Information about the HSM that was created.
    hsm :: Core.Maybe Hsm,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsm', 'createHsmResponse_hsm' - Information about the HSM that was created.
--
-- 'httpStatus', 'createHsmResponse_httpStatus' - The response's http status code.
newCreateHsmResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateHsmResponse
newCreateHsmResponse pHttpStatus_ =
  CreateHsmResponse'
    { hsm = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the HSM that was created.
createHsmResponse_hsm :: Lens.Lens' CreateHsmResponse (Core.Maybe Hsm)
createHsmResponse_hsm = Lens.lens (\CreateHsmResponse' {hsm} -> hsm) (\s@CreateHsmResponse' {} a -> s {hsm = a} :: CreateHsmResponse)

-- | The response's http status code.
createHsmResponse_httpStatus :: Lens.Lens' CreateHsmResponse Core.Int
createHsmResponse_httpStatus = Lens.lens (\CreateHsmResponse' {httpStatus} -> httpStatus) (\s@CreateHsmResponse' {} a -> s {httpStatus = a} :: CreateHsmResponse)

instance Core.NFData CreateHsmResponse

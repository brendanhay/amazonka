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
-- Module      : Network.AWS.DeviceFarm.CreateNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile.
module Network.AWS.DeviceFarm.CreateNetworkProfile
  ( -- * Creating a Request
    CreateNetworkProfile (..),
    newCreateNetworkProfile,

    -- * Request Lenses
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_description,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_type,
    createNetworkProfile_projectArn,
    createNetworkProfile_name,

    -- * Destructuring the Response
    CreateNetworkProfileResponse (..),
    newCreateNetworkProfileResponse,

    -- * Response Lenses
    createNetworkProfileResponse_networkProfile,
    createNetworkProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Core.Maybe Core.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Core.Maybe Core.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Core.Maybe Core.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Core.Maybe Core.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Core.Maybe Core.Natural,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Core.Maybe Core.Natural,
    -- | The description of the network profile.
    description :: Core.Maybe Core.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Core.Maybe Core.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Core.Maybe Core.Integer,
    -- | The type of network profile to create. Valid values are listed here.
    type' :: Core.Maybe NetworkProfileType,
    -- | The Amazon Resource Name (ARN) of the project for which you want to
    -- create a network profile.
    projectArn :: Core.Text,
    -- | The name for the new network profile.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uplinkJitterMs', 'createNetworkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkDelayMs', 'createNetworkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkBandwidthBits', 'createNetworkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'downlinkJitterMs', 'createNetworkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'createNetworkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
--
-- 'downlinkLossPercent', 'createNetworkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'description', 'createNetworkProfile_description' - The description of the network profile.
--
-- 'uplinkDelayMs', 'createNetworkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkBandwidthBits', 'createNetworkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'type'', 'createNetworkProfile_type' - The type of network profile to create. Valid values are listed here.
--
-- 'projectArn', 'createNetworkProfile_projectArn' - The Amazon Resource Name (ARN) of the project for which you want to
-- create a network profile.
--
-- 'name', 'createNetworkProfile_name' - The name for the new network profile.
newCreateNetworkProfile ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateNetworkProfile
newCreateNetworkProfile pProjectArn_ pName_ =
  CreateNetworkProfile'
    { uplinkJitterMs =
        Core.Nothing,
      downlinkDelayMs = Core.Nothing,
      downlinkBandwidthBits = Core.Nothing,
      downlinkJitterMs = Core.Nothing,
      uplinkLossPercent = Core.Nothing,
      downlinkLossPercent = Core.Nothing,
      description = Core.Nothing,
      uplinkDelayMs = Core.Nothing,
      uplinkBandwidthBits = Core.Nothing,
      type' = Core.Nothing,
      projectArn = pProjectArn_,
      name = pName_
    }

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
createNetworkProfile_uplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_uplinkJitterMs = Lens.lens (\CreateNetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@CreateNetworkProfile' {} a -> s {uplinkJitterMs = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_downlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_downlinkDelayMs = Lens.lens (\CreateNetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {downlinkDelayMs = a} :: CreateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_downlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_downlinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: CreateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
createNetworkProfile_downlinkJitterMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_downlinkJitterMs = Lens.lens (\CreateNetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@CreateNetworkProfile' {} a -> s {downlinkJitterMs = a} :: CreateNetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
createNetworkProfile_uplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
createNetworkProfile_uplinkLossPercent = Lens.lens (\CreateNetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@CreateNetworkProfile' {} a -> s {uplinkLossPercent = a} :: CreateNetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
createNetworkProfile_downlinkLossPercent :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Natural)
createNetworkProfile_downlinkLossPercent = Lens.lens (\CreateNetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@CreateNetworkProfile' {} a -> s {downlinkLossPercent = a} :: CreateNetworkProfile)

-- | The description of the network profile.
createNetworkProfile_description :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Text)
createNetworkProfile_description = Lens.lens (\CreateNetworkProfile' {description} -> description) (\s@CreateNetworkProfile' {} a -> s {description = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_uplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_uplinkDelayMs = Lens.lens (\CreateNetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {uplinkDelayMs = a} :: CreateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_uplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Core.Maybe Core.Integer)
createNetworkProfile_uplinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: CreateNetworkProfile)

-- | The type of network profile to create. Valid values are listed here.
createNetworkProfile_type :: Lens.Lens' CreateNetworkProfile (Core.Maybe NetworkProfileType)
createNetworkProfile_type = Lens.lens (\CreateNetworkProfile' {type'} -> type') (\s@CreateNetworkProfile' {} a -> s {type' = a} :: CreateNetworkProfile)

-- | The Amazon Resource Name (ARN) of the project for which you want to
-- create a network profile.
createNetworkProfile_projectArn :: Lens.Lens' CreateNetworkProfile Core.Text
createNetworkProfile_projectArn = Lens.lens (\CreateNetworkProfile' {projectArn} -> projectArn) (\s@CreateNetworkProfile' {} a -> s {projectArn = a} :: CreateNetworkProfile)

-- | The name for the new network profile.
createNetworkProfile_name :: Lens.Lens' CreateNetworkProfile Core.Text
createNetworkProfile_name = Lens.lens (\CreateNetworkProfile' {name} -> name) (\s@CreateNetworkProfile' {} a -> s {name = a} :: CreateNetworkProfile)

instance Core.AWSRequest CreateNetworkProfile where
  type
    AWSResponse CreateNetworkProfile =
      CreateNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Core.<$> (x Core..?> "networkProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNetworkProfile

instance Core.NFData CreateNetworkProfile

instance Core.ToHeaders CreateNetworkProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateNetworkProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("uplinkJitterMs" Core..=) Core.<$> uplinkJitterMs,
            ("downlinkDelayMs" Core..=) Core.<$> downlinkDelayMs,
            ("downlinkBandwidthBits" Core..=)
              Core.<$> downlinkBandwidthBits,
            ("downlinkJitterMs" Core..=)
              Core.<$> downlinkJitterMs,
            ("uplinkLossPercent" Core..=)
              Core.<$> uplinkLossPercent,
            ("downlinkLossPercent" Core..=)
              Core.<$> downlinkLossPercent,
            ("description" Core..=) Core.<$> description,
            ("uplinkDelayMs" Core..=) Core.<$> uplinkDelayMs,
            ("uplinkBandwidthBits" Core..=)
              Core.<$> uplinkBandwidthBits,
            ("type" Core..=) Core.<$> type',
            Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateNetworkProfile where
  toPath = Core.const "/"

instance Core.ToQuery CreateNetworkProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The network profile that is returned by the create network profile
    -- request.
    networkProfile :: Core.Maybe NetworkProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkProfile', 'createNetworkProfileResponse_networkProfile' - The network profile that is returned by the create network profile
-- request.
--
-- 'httpStatus', 'createNetworkProfileResponse_httpStatus' - The response's http status code.
newCreateNetworkProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateNetworkProfileResponse
newCreateNetworkProfileResponse pHttpStatus_ =
  CreateNetworkProfileResponse'
    { networkProfile =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network profile that is returned by the create network profile
-- request.
createNetworkProfileResponse_networkProfile :: Lens.Lens' CreateNetworkProfileResponse (Core.Maybe NetworkProfile)
createNetworkProfileResponse_networkProfile = Lens.lens (\CreateNetworkProfileResponse' {networkProfile} -> networkProfile) (\s@CreateNetworkProfileResponse' {} a -> s {networkProfile = a} :: CreateNetworkProfileResponse)

-- | The response's http status code.
createNetworkProfileResponse_httpStatus :: Lens.Lens' CreateNetworkProfileResponse Core.Int
createNetworkProfileResponse_httpStatus = Lens.lens (\CreateNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkProfileResponse' {} a -> s {httpStatus = a} :: CreateNetworkProfileResponse)

instance Core.NFData CreateNetworkProfileResponse

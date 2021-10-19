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
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_type,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_description,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkBandwidthBits,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The type of network profile to create. Valid values are listed here.
    type' :: Prelude.Maybe NetworkProfileType,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | The description of the network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the project for which you want to
    -- create a network profile.
    projectArn :: Prelude.Text,
    -- | The name for the new network profile.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'uplinkLossPercent', 'createNetworkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
--
-- 'downlinkJitterMs', 'createNetworkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkLossPercent', 'createNetworkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'type'', 'createNetworkProfile_type' - The type of network profile to create. Valid values are listed here.
--
-- 'uplinkDelayMs', 'createNetworkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkBandwidthBits', 'createNetworkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'description', 'createNetworkProfile_description' - The description of the network profile.
--
-- 'downlinkDelayMs', 'createNetworkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkBandwidthBits', 'createNetworkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'projectArn', 'createNetworkProfile_projectArn' - The Amazon Resource Name (ARN) of the project for which you want to
-- create a network profile.
--
-- 'name', 'createNetworkProfile_name' - The name for the new network profile.
newCreateNetworkProfile ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateNetworkProfile
newCreateNetworkProfile pProjectArn_ pName_ =
  CreateNetworkProfile'
    { uplinkJitterMs =
        Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      type' = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      description = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      projectArn = pProjectArn_,
      name = pName_
    }

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
createNetworkProfile_uplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkJitterMs = Lens.lens (\CreateNetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@CreateNetworkProfile' {} a -> s {uplinkJitterMs = a} :: CreateNetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
createNetworkProfile_uplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Natural)
createNetworkProfile_uplinkLossPercent = Lens.lens (\CreateNetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@CreateNetworkProfile' {} a -> s {uplinkLossPercent = a} :: CreateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
createNetworkProfile_downlinkJitterMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_downlinkJitterMs = Lens.lens (\CreateNetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@CreateNetworkProfile' {} a -> s {downlinkJitterMs = a} :: CreateNetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
createNetworkProfile_downlinkLossPercent :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Natural)
createNetworkProfile_downlinkLossPercent = Lens.lens (\CreateNetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@CreateNetworkProfile' {} a -> s {downlinkLossPercent = a} :: CreateNetworkProfile)

-- | The type of network profile to create. Valid values are listed here.
createNetworkProfile_type :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe NetworkProfileType)
createNetworkProfile_type = Lens.lens (\CreateNetworkProfile' {type'} -> type') (\s@CreateNetworkProfile' {} a -> s {type' = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_uplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkDelayMs = Lens.lens (\CreateNetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {uplinkDelayMs = a} :: CreateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_uplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: CreateNetworkProfile)

-- | The description of the network profile.
createNetworkProfile_description :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_description = Lens.lens (\CreateNetworkProfile' {description} -> description) (\s@CreateNetworkProfile' {} a -> s {description = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_downlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_downlinkDelayMs = Lens.lens (\CreateNetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {downlinkDelayMs = a} :: CreateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_downlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_downlinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: CreateNetworkProfile)

-- | The Amazon Resource Name (ARN) of the project for which you want to
-- create a network profile.
createNetworkProfile_projectArn :: Lens.Lens' CreateNetworkProfile Prelude.Text
createNetworkProfile_projectArn = Lens.lens (\CreateNetworkProfile' {projectArn} -> projectArn) (\s@CreateNetworkProfile' {} a -> s {projectArn = a} :: CreateNetworkProfile)

-- | The name for the new network profile.
createNetworkProfile_name :: Lens.Lens' CreateNetworkProfile Prelude.Text
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
            Prelude.<$> (x Core..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkProfile

instance Prelude.NFData CreateNetworkProfile

instance Core.ToHeaders CreateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("uplinkJitterMs" Core..=)
              Prelude.<$> uplinkJitterMs,
            ("uplinkLossPercent" Core..=)
              Prelude.<$> uplinkLossPercent,
            ("downlinkJitterMs" Core..=)
              Prelude.<$> downlinkJitterMs,
            ("downlinkLossPercent" Core..=)
              Prelude.<$> downlinkLossPercent,
            ("type" Core..=) Prelude.<$> type',
            ("uplinkDelayMs" Core..=) Prelude.<$> uplinkDelayMs,
            ("uplinkBandwidthBits" Core..=)
              Prelude.<$> uplinkBandwidthBits,
            ("description" Core..=) Prelude.<$> description,
            ("downlinkDelayMs" Core..=)
              Prelude.<$> downlinkDelayMs,
            ("downlinkBandwidthBits" Core..=)
              Prelude.<$> downlinkBandwidthBits,
            Prelude.Just ("projectArn" Core..= projectArn),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateNetworkProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The network profile that is returned by the create network profile
    -- request.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateNetworkProfileResponse
newCreateNetworkProfileResponse pHttpStatus_ =
  CreateNetworkProfileResponse'
    { networkProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network profile that is returned by the create network profile
-- request.
createNetworkProfileResponse_networkProfile :: Lens.Lens' CreateNetworkProfileResponse (Prelude.Maybe NetworkProfile)
createNetworkProfileResponse_networkProfile = Lens.lens (\CreateNetworkProfileResponse' {networkProfile} -> networkProfile) (\s@CreateNetworkProfileResponse' {} a -> s {networkProfile = a} :: CreateNetworkProfileResponse)

-- | The response's http status code.
createNetworkProfileResponse_httpStatus :: Lens.Lens' CreateNetworkProfileResponse Prelude.Int
createNetworkProfileResponse_httpStatus = Lens.lens (\CreateNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkProfileResponse' {} a -> s {httpStatus = a} :: CreateNetworkProfileResponse)

instance Prelude.NFData CreateNetworkProfileResponse

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
-- Module      : Network.AWS.DeviceFarm.UpdateNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the network profile.
module Network.AWS.DeviceFarm.UpdateNetworkProfile
  ( -- * Creating a Request
    UpdateNetworkProfile (..),
    newUpdateNetworkProfile,

    -- * Request Lenses
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_name,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_type,
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_description,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_arn,

    -- * Destructuring the Response
    UpdateNetworkProfileResponse (..),
    newUpdateNetworkProfileResponse,

    -- * Response Lenses
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | The name of the network profile about which you are returning
    -- information.
    name :: Prelude.Maybe Prelude.Text,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The type of network profile to return information about. Valid values
    -- are listed here.
    type' :: Prelude.Maybe NetworkProfileType,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | The description of the network profile about which you are returning
    -- information.
    description :: Prelude.Maybe Prelude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the project for which you want to
    -- update network profile settings.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uplinkJitterMs', 'updateNetworkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'updateNetworkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
--
-- 'downlinkJitterMs', 'updateNetworkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'name', 'updateNetworkProfile_name' - The name of the network profile about which you are returning
-- information.
--
-- 'downlinkLossPercent', 'updateNetworkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'type'', 'updateNetworkProfile_type' - The type of network profile to return information about. Valid values
-- are listed here.
--
-- 'uplinkDelayMs', 'updateNetworkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkBandwidthBits', 'updateNetworkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'description', 'updateNetworkProfile_description' - The description of the network profile about which you are returning
-- information.
--
-- 'downlinkDelayMs', 'updateNetworkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkBandwidthBits', 'updateNetworkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'arn', 'updateNetworkProfile_arn' - The Amazon Resource Name (ARN) of the project for which you want to
-- update network profile settings.
newUpdateNetworkProfile ::
  -- | 'arn'
  Prelude.Text ->
  UpdateNetworkProfile
newUpdateNetworkProfile pArn_ =
  UpdateNetworkProfile'
    { uplinkJitterMs =
        Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      name = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      type' = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      description = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      arn = pArn_
    }

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
updateNetworkProfile_uplinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkJitterMs = Lens.lens (\UpdateNetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@UpdateNetworkProfile' {} a -> s {uplinkJitterMs = a} :: UpdateNetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
updateNetworkProfile_uplinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Natural)
updateNetworkProfile_uplinkLossPercent = Lens.lens (\UpdateNetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@UpdateNetworkProfile' {} a -> s {uplinkLossPercent = a} :: UpdateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
updateNetworkProfile_downlinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkJitterMs = Lens.lens (\UpdateNetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@UpdateNetworkProfile' {} a -> s {downlinkJitterMs = a} :: UpdateNetworkProfile)

-- | The name of the network profile about which you are returning
-- information.
updateNetworkProfile_name :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Text)
updateNetworkProfile_name = Lens.lens (\UpdateNetworkProfile' {name} -> name) (\s@UpdateNetworkProfile' {} a -> s {name = a} :: UpdateNetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
updateNetworkProfile_downlinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Natural)
updateNetworkProfile_downlinkLossPercent = Lens.lens (\UpdateNetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@UpdateNetworkProfile' {} a -> s {downlinkLossPercent = a} :: UpdateNetworkProfile)

-- | The type of network profile to return information about. Valid values
-- are listed here.
updateNetworkProfile_type :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe NetworkProfileType)
updateNetworkProfile_type = Lens.lens (\UpdateNetworkProfile' {type'} -> type') (\s@UpdateNetworkProfile' {} a -> s {type' = a} :: UpdateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
updateNetworkProfile_uplinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkDelayMs = Lens.lens (\UpdateNetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@UpdateNetworkProfile' {} a -> s {uplinkDelayMs = a} :: UpdateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
updateNetworkProfile_uplinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkBandwidthBits = Lens.lens (\UpdateNetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@UpdateNetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: UpdateNetworkProfile)

-- | The description of the network profile about which you are returning
-- information.
updateNetworkProfile_description :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Text)
updateNetworkProfile_description = Lens.lens (\UpdateNetworkProfile' {description} -> description) (\s@UpdateNetworkProfile' {} a -> s {description = a} :: UpdateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
updateNetworkProfile_downlinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkDelayMs = Lens.lens (\UpdateNetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@UpdateNetworkProfile' {} a -> s {downlinkDelayMs = a} :: UpdateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
updateNetworkProfile_downlinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkBandwidthBits = Lens.lens (\UpdateNetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@UpdateNetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: UpdateNetworkProfile)

-- | The Amazon Resource Name (ARN) of the project for which you want to
-- update network profile settings.
updateNetworkProfile_arn :: Lens.Lens' UpdateNetworkProfile Prelude.Text
updateNetworkProfile_arn = Lens.lens (\UpdateNetworkProfile' {arn} -> arn) (\s@UpdateNetworkProfile' {} a -> s {arn = a} :: UpdateNetworkProfile)

instance Core.AWSRequest UpdateNetworkProfile where
  type
    AWSResponse UpdateNetworkProfile =
      UpdateNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Prelude.<$> (x Core..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNetworkProfile

instance Prelude.NFData UpdateNetworkProfile

instance Core.ToHeaders UpdateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("uplinkJitterMs" Core..=)
              Prelude.<$> uplinkJitterMs,
            ("uplinkLossPercent" Core..=)
              Prelude.<$> uplinkLossPercent,
            ("downlinkJitterMs" Core..=)
              Prelude.<$> downlinkJitterMs,
            ("name" Core..=) Prelude.<$> name,
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
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateNetworkProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNetworkProfileResponse' smart constructor.
data UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { -- | A list of the available network profiles.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkProfile', 'updateNetworkProfileResponse_networkProfile' - A list of the available network profiles.
--
-- 'httpStatus', 'updateNetworkProfileResponse_httpStatus' - The response's http status code.
newUpdateNetworkProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNetworkProfileResponse
newUpdateNetworkProfileResponse pHttpStatus_ =
  UpdateNetworkProfileResponse'
    { networkProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the available network profiles.
updateNetworkProfileResponse_networkProfile :: Lens.Lens' UpdateNetworkProfileResponse (Prelude.Maybe NetworkProfile)
updateNetworkProfileResponse_networkProfile = Lens.lens (\UpdateNetworkProfileResponse' {networkProfile} -> networkProfile) (\s@UpdateNetworkProfileResponse' {} a -> s {networkProfile = a} :: UpdateNetworkProfileResponse)

-- | The response's http status code.
updateNetworkProfileResponse_httpStatus :: Lens.Lens' UpdateNetworkProfileResponse Prelude.Int
updateNetworkProfileResponse_httpStatus = Lens.lens (\UpdateNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateNetworkProfileResponse' {} a -> s {httpStatus = a} :: UpdateNetworkProfileResponse)

instance Prelude.NFData UpdateNetworkProfileResponse

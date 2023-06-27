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
-- Module      : Amazonka.DeviceFarm.UpdateNetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the network profile.
module Amazonka.DeviceFarm.UpdateNetworkProfile
  ( -- * Creating a Request
    UpdateNetworkProfile (..),
    newUpdateNetworkProfile,

    -- * Request Lenses
    updateNetworkProfile_description,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_name,
    updateNetworkProfile_type,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_arn,

    -- * Destructuring the Response
    UpdateNetworkProfileResponse (..),
    newUpdateNetworkProfileResponse,

    -- * Response Lenses
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { -- | The description of the network profile about which you are returning
    -- information.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The name of the network profile about which you are returning
    -- information.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of network profile to return information about. Valid values
    -- are listed here.
    type' :: Prelude.Maybe NetworkProfileType,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Prelude.Maybe Prelude.Natural,
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
-- 'description', 'updateNetworkProfile_description' - The description of the network profile about which you are returning
-- information.
--
-- 'downlinkBandwidthBits', 'updateNetworkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'downlinkDelayMs', 'updateNetworkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkJitterMs', 'updateNetworkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkLossPercent', 'updateNetworkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'name', 'updateNetworkProfile_name' - The name of the network profile about which you are returning
-- information.
--
-- 'type'', 'updateNetworkProfile_type' - The type of network profile to return information about. Valid values
-- are listed here.
--
-- 'uplinkBandwidthBits', 'updateNetworkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'uplinkDelayMs', 'updateNetworkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkJitterMs', 'updateNetworkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'updateNetworkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
--
-- 'arn', 'updateNetworkProfile_arn' - The Amazon Resource Name (ARN) of the project for which you want to
-- update network profile settings.
newUpdateNetworkProfile ::
  -- | 'arn'
  Prelude.Text ->
  UpdateNetworkProfile
newUpdateNetworkProfile pArn_ =
  UpdateNetworkProfile'
    { description =
        Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkJitterMs = Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing,
      arn = pArn_
    }

-- | The description of the network profile about which you are returning
-- information.
updateNetworkProfile_description :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Text)
updateNetworkProfile_description = Lens.lens (\UpdateNetworkProfile' {description} -> description) (\s@UpdateNetworkProfile' {} a -> s {description = a} :: UpdateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
updateNetworkProfile_downlinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkBandwidthBits = Lens.lens (\UpdateNetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@UpdateNetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: UpdateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
updateNetworkProfile_downlinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkDelayMs = Lens.lens (\UpdateNetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@UpdateNetworkProfile' {} a -> s {downlinkDelayMs = a} :: UpdateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
updateNetworkProfile_downlinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_downlinkJitterMs = Lens.lens (\UpdateNetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@UpdateNetworkProfile' {} a -> s {downlinkJitterMs = a} :: UpdateNetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
updateNetworkProfile_downlinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Natural)
updateNetworkProfile_downlinkLossPercent = Lens.lens (\UpdateNetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@UpdateNetworkProfile' {} a -> s {downlinkLossPercent = a} :: UpdateNetworkProfile)

-- | The name of the network profile about which you are returning
-- information.
updateNetworkProfile_name :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Text)
updateNetworkProfile_name = Lens.lens (\UpdateNetworkProfile' {name} -> name) (\s@UpdateNetworkProfile' {} a -> s {name = a} :: UpdateNetworkProfile)

-- | The type of network profile to return information about. Valid values
-- are listed here.
updateNetworkProfile_type :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe NetworkProfileType)
updateNetworkProfile_type = Lens.lens (\UpdateNetworkProfile' {type'} -> type') (\s@UpdateNetworkProfile' {} a -> s {type' = a} :: UpdateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
updateNetworkProfile_uplinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkBandwidthBits = Lens.lens (\UpdateNetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@UpdateNetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: UpdateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
updateNetworkProfile_uplinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkDelayMs = Lens.lens (\UpdateNetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@UpdateNetworkProfile' {} a -> s {uplinkDelayMs = a} :: UpdateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
updateNetworkProfile_uplinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Integer)
updateNetworkProfile_uplinkJitterMs = Lens.lens (\UpdateNetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@UpdateNetworkProfile' {} a -> s {uplinkJitterMs = a} :: UpdateNetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
updateNetworkProfile_uplinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Prelude.Maybe Prelude.Natural)
updateNetworkProfile_uplinkLossPercent = Lens.lens (\UpdateNetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@UpdateNetworkProfile' {} a -> s {uplinkLossPercent = a} :: UpdateNetworkProfile)

-- | The Amazon Resource Name (ARN) of the project for which you want to
-- update network profile settings.
updateNetworkProfile_arn :: Lens.Lens' UpdateNetworkProfile Prelude.Text
updateNetworkProfile_arn = Lens.lens (\UpdateNetworkProfile' {arn} -> arn) (\s@UpdateNetworkProfile' {} a -> s {arn = a} :: UpdateNetworkProfile)

instance Core.AWSRequest UpdateNetworkProfile where
  type
    AWSResponse UpdateNetworkProfile =
      UpdateNetworkProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Prelude.<$> (x Data..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNetworkProfile where
  hashWithSalt _salt UpdateNetworkProfile' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` downlinkBandwidthBits
      `Prelude.hashWithSalt` downlinkDelayMs
      `Prelude.hashWithSalt` downlinkJitterMs
      `Prelude.hashWithSalt` downlinkLossPercent
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` uplinkBandwidthBits
      `Prelude.hashWithSalt` uplinkDelayMs
      `Prelude.hashWithSalt` uplinkJitterMs
      `Prelude.hashWithSalt` uplinkLossPercent
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateNetworkProfile where
  rnf UpdateNetworkProfile' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf downlinkBandwidthBits
      `Prelude.seq` Prelude.rnf downlinkDelayMs
      `Prelude.seq` Prelude.rnf downlinkJitterMs
      `Prelude.seq` Prelude.rnf downlinkLossPercent
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf uplinkBandwidthBits
      `Prelude.seq` Prelude.rnf uplinkDelayMs
      `Prelude.seq` Prelude.rnf uplinkJitterMs
      `Prelude.seq` Prelude.rnf uplinkLossPercent
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.UpdateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("downlinkBandwidthBits" Data..=)
              Prelude.<$> downlinkBandwidthBits,
            ("downlinkDelayMs" Data..=)
              Prelude.<$> downlinkDelayMs,
            ("downlinkJitterMs" Data..=)
              Prelude.<$> downlinkJitterMs,
            ("downlinkLossPercent" Data..=)
              Prelude.<$> downlinkLossPercent,
            ("name" Data..=) Prelude.<$> name,
            ("type" Data..=) Prelude.<$> type',
            ("uplinkBandwidthBits" Data..=)
              Prelude.<$> uplinkBandwidthBits,
            ("uplinkDelayMs" Data..=) Prelude.<$> uplinkDelayMs,
            ("uplinkJitterMs" Data..=)
              Prelude.<$> uplinkJitterMs,
            ("uplinkLossPercent" Data..=)
              Prelude.<$> uplinkLossPercent,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateNetworkProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNetworkProfile where
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

instance Prelude.NFData UpdateNetworkProfileResponse where
  rnf UpdateNetworkProfileResponse' {..} =
    Prelude.rnf networkProfile
      `Prelude.seq` Prelude.rnf httpStatus

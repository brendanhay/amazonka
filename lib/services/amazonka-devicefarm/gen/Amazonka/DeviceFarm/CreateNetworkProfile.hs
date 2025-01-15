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
-- Module      : Amazonka.DeviceFarm.CreateNetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile.
module Amazonka.DeviceFarm.CreateNetworkProfile
  ( -- * Creating a Request
    CreateNetworkProfile (..),
    newCreateNetworkProfile,

    -- * Request Lenses
    createNetworkProfile_description,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_type,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_uplinkLossPercent,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The description of the network profile.
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
    -- | The type of network profile to create. Valid values are listed here.
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
-- 'description', 'createNetworkProfile_description' - The description of the network profile.
--
-- 'downlinkBandwidthBits', 'createNetworkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'downlinkDelayMs', 'createNetworkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkJitterMs', 'createNetworkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkLossPercent', 'createNetworkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'type'', 'createNetworkProfile_type' - The type of network profile to create. Valid values are listed here.
--
-- 'uplinkBandwidthBits', 'createNetworkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'uplinkDelayMs', 'createNetworkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkJitterMs', 'createNetworkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'createNetworkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
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
    { description =
        Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      type' = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkJitterMs = Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing,
      projectArn = pProjectArn_,
      name = pName_
    }

-- | The description of the network profile.
createNetworkProfile_description :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_description = Lens.lens (\CreateNetworkProfile' {description} -> description) (\s@CreateNetworkProfile' {} a -> s {description = a} :: CreateNetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_downlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_downlinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_downlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_downlinkDelayMs = Lens.lens (\CreateNetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {downlinkDelayMs = a} :: CreateNetworkProfile)

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

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
createNetworkProfile_uplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkBandwidthBits = Lens.lens (\CreateNetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@CreateNetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: CreateNetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
createNetworkProfile_uplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkDelayMs = Lens.lens (\CreateNetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@CreateNetworkProfile' {} a -> s {uplinkDelayMs = a} :: CreateNetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
createNetworkProfile_uplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Integer)
createNetworkProfile_uplinkJitterMs = Lens.lens (\CreateNetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@CreateNetworkProfile' {} a -> s {uplinkJitterMs = a} :: CreateNetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
createNetworkProfile_uplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Natural)
createNetworkProfile_uplinkLossPercent = Lens.lens (\CreateNetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@CreateNetworkProfile' {} a -> s {uplinkLossPercent = a} :: CreateNetworkProfile)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Prelude.<$> (x Data..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkProfile where
  hashWithSalt _salt CreateNetworkProfile' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` downlinkBandwidthBits
      `Prelude.hashWithSalt` downlinkDelayMs
      `Prelude.hashWithSalt` downlinkJitterMs
      `Prelude.hashWithSalt` downlinkLossPercent
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` uplinkBandwidthBits
      `Prelude.hashWithSalt` uplinkDelayMs
      `Prelude.hashWithSalt` uplinkJitterMs
      `Prelude.hashWithSalt` uplinkLossPercent
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateNetworkProfile where
  rnf CreateNetworkProfile' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf downlinkBandwidthBits `Prelude.seq`
        Prelude.rnf downlinkDelayMs `Prelude.seq`
          Prelude.rnf downlinkJitterMs `Prelude.seq`
            Prelude.rnf downlinkLossPercent `Prelude.seq`
              Prelude.rnf type' `Prelude.seq`
                Prelude.rnf uplinkBandwidthBits `Prelude.seq`
                  Prelude.rnf uplinkDelayMs `Prelude.seq`
                    Prelude.rnf uplinkJitterMs `Prelude.seq`
                      Prelude.rnf uplinkLossPercent `Prelude.seq`
                        Prelude.rnf projectArn `Prelude.seq`
                          Prelude.rnf name

instance Data.ToHeaders CreateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.CreateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
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
            ("type" Data..=) Prelude.<$> type',
            ("uplinkBandwidthBits" Data..=)
              Prelude.<$> uplinkBandwidthBits,
            ("uplinkDelayMs" Data..=) Prelude.<$> uplinkDelayMs,
            ("uplinkJitterMs" Data..=)
              Prelude.<$> uplinkJitterMs,
            ("uplinkLossPercent" Data..=)
              Prelude.<$> uplinkLossPercent,
            Prelude.Just ("projectArn" Data..= projectArn),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateNetworkProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNetworkProfile where
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

instance Prelude.NFData CreateNetworkProfileResponse where
  rnf CreateNetworkProfileResponse' {..} =
    Prelude.rnf networkProfile `Prelude.seq`
      Prelude.rnf httpStatus

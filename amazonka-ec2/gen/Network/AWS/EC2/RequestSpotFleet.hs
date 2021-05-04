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
-- Module      : Network.AWS.EC2.RequestSpotFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Fleet request.
--
-- The Spot Fleet request specifies the total target capacity and the
-- On-Demand target capacity. Amazon EC2 calculates the difference between
-- the total capacity and On-Demand capacity, and launches the difference
-- as Spot capacity.
--
-- You can submit a single request that includes multiple launch
-- specifications that vary by instance type, AMI, Availability Zone, or
-- subnet.
--
-- By default, the Spot Fleet requests Spot Instances in the Spot Instance
-- pool where the price per unit is the lowest. Each launch specification
-- can include its own instance weighting that reflects the value of the
-- instance type to your application workload.
--
-- Alternatively, you can specify that the Spot Fleet distribute the target
-- capacity across the Spot pools included in its launch specifications. By
-- ensuring that the Spot Instances in your Spot Fleet are in different
-- Spot pools, you can improve the availability of your fleet.
--
-- You can specify tags for the Spot Fleet request and instances launched
-- by the fleet. You cannot tag other resource types in a Spot Fleet
-- request because only the @spot-fleet-request@ and @instance@ resource
-- types are supported.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html Spot Fleet requests>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Network.AWS.EC2.RequestSpotFleet
  ( -- * Creating a Request
    RequestSpotFleet (..),
    newRequestSpotFleet,

    -- * Request Lenses
    requestSpotFleet_dryRun,
    requestSpotFleet_spotFleetRequestConfig,

    -- * Destructuring the Response
    RequestSpotFleetResponse (..),
    newRequestSpotFleetResponse,

    -- * Response Lenses
    requestSpotFleetResponse_spotFleetRequestId,
    requestSpotFleetResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'newRequestSpotFleet' smart constructor.
data RequestSpotFleet = RequestSpotFleet'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The configuration for the Spot Fleet request.
    spotFleetRequestConfig :: SpotFleetRequestConfigData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'requestSpotFleet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'spotFleetRequestConfig', 'requestSpotFleet_spotFleetRequestConfig' - The configuration for the Spot Fleet request.
newRequestSpotFleet ::
  -- | 'spotFleetRequestConfig'
  SpotFleetRequestConfigData ->
  RequestSpotFleet
newRequestSpotFleet pSpotFleetRequestConfig_ =
  RequestSpotFleet'
    { dryRun = Prelude.Nothing,
      spotFleetRequestConfig = pSpotFleetRequestConfig_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
requestSpotFleet_dryRun :: Lens.Lens' RequestSpotFleet (Prelude.Maybe Prelude.Bool)
requestSpotFleet_dryRun = Lens.lens (\RequestSpotFleet' {dryRun} -> dryRun) (\s@RequestSpotFleet' {} a -> s {dryRun = a} :: RequestSpotFleet)

-- | The configuration for the Spot Fleet request.
requestSpotFleet_spotFleetRequestConfig :: Lens.Lens' RequestSpotFleet SpotFleetRequestConfigData
requestSpotFleet_spotFleetRequestConfig = Lens.lens (\RequestSpotFleet' {spotFleetRequestConfig} -> spotFleetRequestConfig) (\s@RequestSpotFleet' {} a -> s {spotFleetRequestConfig = a} :: RequestSpotFleet)

instance Prelude.AWSRequest RequestSpotFleet where
  type Rs RequestSpotFleet = RequestSpotFleetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RequestSpotFleetResponse'
            Prelude.<$> (x Prelude..@? "spotFleetRequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestSpotFleet

instance Prelude.NFData RequestSpotFleet

instance Prelude.ToHeaders RequestSpotFleet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RequestSpotFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RequestSpotFleet where
  toQuery RequestSpotFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RequestSpotFleet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "SpotFleetRequestConfig"
          Prelude.=: spotFleetRequestConfig
      ]

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'newRequestSpotFleetResponse' smart constructor.
data RequestSpotFleetResponse = RequestSpotFleetResponse'
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotFleetRequestId', 'requestSpotFleetResponse_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'httpStatus', 'requestSpotFleetResponse_httpStatus' - The response's http status code.
newRequestSpotFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RequestSpotFleetResponse
newRequestSpotFleetResponse pHttpStatus_ =
  RequestSpotFleetResponse'
    { spotFleetRequestId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Spot Fleet request.
requestSpotFleetResponse_spotFleetRequestId :: Lens.Lens' RequestSpotFleetResponse (Prelude.Maybe Prelude.Text)
requestSpotFleetResponse_spotFleetRequestId = Lens.lens (\RequestSpotFleetResponse' {spotFleetRequestId} -> spotFleetRequestId) (\s@RequestSpotFleetResponse' {} a -> s {spotFleetRequestId = a} :: RequestSpotFleetResponse)

-- | The response's http status code.
requestSpotFleetResponse_httpStatus :: Lens.Lens' RequestSpotFleetResponse Prelude.Int
requestSpotFleetResponse_httpStatus = Lens.lens (\RequestSpotFleetResponse' {httpStatus} -> httpStatus) (\s@RequestSpotFleetResponse' {} a -> s {httpStatus = a} :: RequestSpotFleetResponse)

instance Prelude.NFData RequestSpotFleetResponse

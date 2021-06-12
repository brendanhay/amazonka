{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateHttpTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import qualified Network.AWS.Lens as Lens

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newLaunchTemplateInstanceMetadataOptionsRequest' smart constructor.
data LaunchTemplateInstanceMetadataOptionsRequest = LaunchTemplateInstanceMetadataOptionsRequest'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your
    -- instances. If the parameter is not specified, the default state is
    -- @enabled@.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Core.Maybe LaunchTemplateInstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Core.Maybe Core.Int,
    -- | The state of token usage for your instance metadata requests. If the
    -- parameter is not specified in the request, the default state is
    -- @optional@.
    --
    -- If the state is @optional@, you can choose to retrieve instance metadata
    -- with or without a signed token header on your request. If you retrieve
    -- the IAM role credentials without a token, the version 1.0 role
    -- credentials are returned. If you retrieve the IAM role credentials using
    -- a valid signed token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a signed token header with any
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credentials always returns the version 2.0 credentials; the version
    -- 1.0 credentials are not available.
    httpTokens :: Core.Maybe LaunchTemplateHttpTokensState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMetadataOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'launchTemplateInstanceMetadataOptionsRequest_httpEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
--
-- 'httpPutResponseHopLimit', 'launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'launchTemplateInstanceMetadataOptionsRequest_httpTokens' - The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
newLaunchTemplateInstanceMetadataOptionsRequest ::
  LaunchTemplateInstanceMetadataOptionsRequest
newLaunchTemplateInstanceMetadataOptionsRequest =
  LaunchTemplateInstanceMetadataOptionsRequest'
    { httpEndpoint =
        Core.Nothing,
      httpPutResponseHopLimit =
        Core.Nothing,
      httpTokens = Core.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
launchTemplateInstanceMetadataOptionsRequest_httpEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe LaunchTemplateInstanceMetadataEndpointState)
launchTemplateInstanceMetadataOptionsRequest_httpEndpoint = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpEndpoint} -> httpEndpoint) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpEndpoint = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe Core.Int)
launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpPutResponseHopLimit = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

-- | The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
launchTemplateInstanceMetadataOptionsRequest_httpTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe LaunchTemplateHttpTokensState)
launchTemplateInstanceMetadataOptionsRequest_httpTokens = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpTokens} -> httpTokens) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpTokens = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

instance
  Core.Hashable
    LaunchTemplateInstanceMetadataOptionsRequest

instance
  Core.NFData
    LaunchTemplateInstanceMetadataOptionsRequest

instance
  Core.ToQuery
    LaunchTemplateInstanceMetadataOptionsRequest
  where
  toQuery
    LaunchTemplateInstanceMetadataOptionsRequest' {..} =
      Core.mconcat
        [ "HttpEndpoint" Core.=: httpEndpoint,
          "HttpPutResponseHopLimit"
            Core.=: httpPutResponseHopLimit,
          "HttpTokens" Core.=: httpTokens
        ]

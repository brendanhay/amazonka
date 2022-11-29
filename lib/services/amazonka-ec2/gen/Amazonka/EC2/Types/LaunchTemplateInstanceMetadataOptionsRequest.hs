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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateHttpTokensState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataProtocolIpv6
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataTagsState
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newLaunchTemplateInstanceMetadataOptionsRequest' smart constructor.
data LaunchTemplateInstanceMetadataOptionsRequest = LaunchTemplateInstanceMetadataOptionsRequest'
  { -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: @1@
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
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
    httpTokens :: Prelude.Maybe LaunchTemplateHttpTokensState,
    -- | Enables or disables the HTTP metadata endpoint on your instances. If the
    -- parameter is not specified, the default state is @enabled@.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Prelude.Maybe LaunchTemplateInstanceMetadataEndpointState,
    -- | Set to @enabled@ to allow access to instance tags from the instance
    -- metadata. Set to @disabled@ to turn off access to instance tags from the
    -- instance metadata. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
    --
    -- Default: @disabled@
    instanceMetadataTags :: Prelude.Maybe LaunchTemplateInstanceMetadataTagsState,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    --
    -- Default: @disabled@
    httpProtocolIpv6 :: Prelude.Maybe LaunchTemplateInstanceMetadataProtocolIpv6
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMetadataOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpPutResponseHopLimit', 'launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: @1@
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
--
-- 'httpEndpoint', 'launchTemplateInstanceMetadataOptionsRequest_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances. If the
-- parameter is not specified, the default state is @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
--
-- 'instanceMetadataTags', 'launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags' - Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
--
-- 'httpProtocolIpv6', 'launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
--
-- Default: @disabled@
newLaunchTemplateInstanceMetadataOptionsRequest ::
  LaunchTemplateInstanceMetadataOptionsRequest
newLaunchTemplateInstanceMetadataOptionsRequest =
  LaunchTemplateInstanceMetadataOptionsRequest'
    { httpPutResponseHopLimit =
        Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      httpEndpoint =
        Prelude.Nothing,
      instanceMetadataTags =
        Prelude.Nothing,
      httpProtocolIpv6 =
        Prelude.Nothing
    }

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: @1@
--
-- Possible values: Integers from 1 to 64
launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Prelude.Maybe Prelude.Int)
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
launchTemplateInstanceMetadataOptionsRequest_httpTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Prelude.Maybe LaunchTemplateHttpTokensState)
launchTemplateInstanceMetadataOptionsRequest_httpTokens = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpTokens} -> httpTokens) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpTokens = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

-- | Enables or disables the HTTP metadata endpoint on your instances. If the
-- parameter is not specified, the default state is @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
launchTemplateInstanceMetadataOptionsRequest_httpEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Prelude.Maybe LaunchTemplateInstanceMetadataEndpointState)
launchTemplateInstanceMetadataOptionsRequest_httpEndpoint = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpEndpoint} -> httpEndpoint) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpEndpoint = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

-- | Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Prelude.Maybe LaunchTemplateInstanceMetadataTagsState)
launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {instanceMetadataTags} -> instanceMetadataTags) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {instanceMetadataTags = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
--
-- Default: @disabled@
launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6 :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Prelude.Maybe LaunchTemplateInstanceMetadataProtocolIpv6)
launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6 = Lens.lens (\LaunchTemplateInstanceMetadataOptionsRequest' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@LaunchTemplateInstanceMetadataOptionsRequest' {} a -> s {httpProtocolIpv6 = a} :: LaunchTemplateInstanceMetadataOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateInstanceMetadataOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceMetadataOptionsRequest' {..} =
      _salt
        `Prelude.hashWithSalt` httpPutResponseHopLimit
        `Prelude.hashWithSalt` httpTokens
        `Prelude.hashWithSalt` httpEndpoint
        `Prelude.hashWithSalt` instanceMetadataTags
        `Prelude.hashWithSalt` httpProtocolIpv6

instance
  Prelude.NFData
    LaunchTemplateInstanceMetadataOptionsRequest
  where
  rnf LaunchTemplateInstanceMetadataOptionsRequest' {..} =
    Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf instanceMetadataTags
      `Prelude.seq` Prelude.rnf httpProtocolIpv6

instance
  Core.ToQuery
    LaunchTemplateInstanceMetadataOptionsRequest
  where
  toQuery
    LaunchTemplateInstanceMetadataOptionsRequest' {..} =
      Prelude.mconcat
        [ "HttpPutResponseHopLimit"
            Core.=: httpPutResponseHopLimit,
          "HttpTokens" Core.=: httpTokens,
          "HttpEndpoint" Core.=: httpEndpoint,
          "InstanceMetadataTags" Core.=: instanceMetadataTags,
          "HttpProtocolIpv6" Core.=: httpProtocolIpv6
        ]

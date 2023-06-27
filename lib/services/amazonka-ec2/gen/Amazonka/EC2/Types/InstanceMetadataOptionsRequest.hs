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
-- Module      : Amazonka.EC2.Types.InstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMetadataOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HttpTokensState
import Amazonka.EC2.Types.InstanceMetadataEndpointState
import Amazonka.EC2.Types.InstanceMetadataProtocolState
import Amazonka.EC2.Types.InstanceMetadataTagsState
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instance.
--
-- /See:/ 'newInstanceMetadataOptionsRequest' smart constructor.
data InstanceMetadataOptionsRequest = InstanceMetadataOptionsRequest'
  { -- | Enables or disables the HTTP metadata endpoint on your instances.
    --
    -- If you specify a value of @disabled@, you cannot access your instance
    -- metadata.
    --
    -- Default: @enabled@
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    httpProtocolIpv6 :: Prelude.Maybe InstanceMetadataProtocolState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
    -- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
    -- @required@ (in other words, set the use of IMDSv2 to @required@).
    --
    -- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
    --     instance metadata with or without a session token in your request.
    --     If you retrieve the IAM role credentials without a token, the IMDSv1
    --     role credentials are returned. If you retrieve the IAM role
    --     credentials using a valid session token, the IMDSv2 role credentials
    --     are returned.
    --
    -- -   @required@ - When IMDSv2 is required, you must send a session token
    --     with any instance metadata retrieval requests. In this state,
    --     retrieving the IAM role credentials always returns IMDSv2
    --     credentials; IMDSv1 credentials are not available.
    --
    -- Default: @optional@
    httpTokens :: Prelude.Maybe HttpTokensState,
    -- | Set to @enabled@ to allow access to instance tags from the instance
    -- metadata. Set to @disabled@ to turn off access to instance tags from the
    -- instance metadata. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
    --
    -- Default: @disabled@
    instanceMetadataTags :: Prelude.Maybe InstanceMetadataTagsState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'instanceMetadataOptionsRequest_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
--
-- Default: @enabled@
--
-- 'httpProtocolIpv6', 'instanceMetadataOptionsRequest_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
--
-- 'httpPutResponseHopLimit', 'instanceMetadataOptionsRequest_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'instanceMetadataOptionsRequest_httpTokens' - IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
-- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
-- @required@ (in other words, set the use of IMDSv2 to @required@).
--
-- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
--     instance metadata with or without a session token in your request.
--     If you retrieve the IAM role credentials without a token, the IMDSv1
--     role credentials are returned. If you retrieve the IAM role
--     credentials using a valid session token, the IMDSv2 role credentials
--     are returned.
--
-- -   @required@ - When IMDSv2 is required, you must send a session token
--     with any instance metadata retrieval requests. In this state,
--     retrieving the IAM role credentials always returns IMDSv2
--     credentials; IMDSv1 credentials are not available.
--
-- Default: @optional@
--
-- 'instanceMetadataTags', 'instanceMetadataOptionsRequest_instanceMetadataTags' - Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
newInstanceMetadataOptionsRequest ::
  InstanceMetadataOptionsRequest
newInstanceMetadataOptionsRequest =
  InstanceMetadataOptionsRequest'
    { httpEndpoint =
        Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      instanceMetadataTags = Prelude.Nothing
    }

-- | Enables or disables the HTTP metadata endpoint on your instances.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
--
-- Default: @enabled@
instanceMetadataOptionsRequest_httpEndpoint :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe InstanceMetadataEndpointState)
instanceMetadataOptionsRequest_httpEndpoint = Lens.lens (\InstanceMetadataOptionsRequest' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptionsRequest)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
instanceMetadataOptionsRequest_httpProtocolIpv6 :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe InstanceMetadataProtocolState)
instanceMetadataOptionsRequest_httpProtocolIpv6 = Lens.lens (\InstanceMetadataOptionsRequest' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpProtocolIpv6 = a} :: InstanceMetadataOptionsRequest)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
instanceMetadataOptionsRequest_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe Prelude.Int)
instanceMetadataOptionsRequest_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptionsRequest' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptionsRequest)

-- | IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
-- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
-- @required@ (in other words, set the use of IMDSv2 to @required@).
--
-- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
--     instance metadata with or without a session token in your request.
--     If you retrieve the IAM role credentials without a token, the IMDSv1
--     role credentials are returned. If you retrieve the IAM role
--     credentials using a valid session token, the IMDSv2 role credentials
--     are returned.
--
-- -   @required@ - When IMDSv2 is required, you must send a session token
--     with any instance metadata retrieval requests. In this state,
--     retrieving the IAM role credentials always returns IMDSv2
--     credentials; IMDSv1 credentials are not available.
--
-- Default: @optional@
instanceMetadataOptionsRequest_httpTokens :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe HttpTokensState)
instanceMetadataOptionsRequest_httpTokens = Lens.lens (\InstanceMetadataOptionsRequest' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpTokens = a} :: InstanceMetadataOptionsRequest)

-- | Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
instanceMetadataOptionsRequest_instanceMetadataTags :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe InstanceMetadataTagsState)
instanceMetadataOptionsRequest_instanceMetadataTags = Lens.lens (\InstanceMetadataOptionsRequest' {instanceMetadataTags} -> instanceMetadataTags) (\s@InstanceMetadataOptionsRequest' {} a -> s {instanceMetadataTags = a} :: InstanceMetadataOptionsRequest)

instance
  Prelude.Hashable
    InstanceMetadataOptionsRequest
  where
  hashWithSalt
    _salt
    InstanceMetadataOptionsRequest' {..} =
      _salt
        `Prelude.hashWithSalt` httpEndpoint
        `Prelude.hashWithSalt` httpProtocolIpv6
        `Prelude.hashWithSalt` httpPutResponseHopLimit
        `Prelude.hashWithSalt` httpTokens
        `Prelude.hashWithSalt` instanceMetadataTags

instance
  Prelude.NFData
    InstanceMetadataOptionsRequest
  where
  rnf InstanceMetadataOptionsRequest' {..} =
    Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf instanceMetadataTags

instance Data.ToQuery InstanceMetadataOptionsRequest where
  toQuery InstanceMetadataOptionsRequest' {..} =
    Prelude.mconcat
      [ "HttpEndpoint" Data.=: httpEndpoint,
        "HttpProtocolIpv6" Data.=: httpProtocolIpv6,
        "HttpPutResponseHopLimit"
          Data.=: httpPutResponseHopLimit,
        "HttpTokens" Data.=: httpTokens,
        "InstanceMetadataTags" Data.=: instanceMetadataTags
      ]

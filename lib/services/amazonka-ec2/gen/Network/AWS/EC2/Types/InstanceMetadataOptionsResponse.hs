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
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.HttpTokensState
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import Network.AWS.EC2.Types.InstanceMetadataOptionsState
import Network.AWS.EC2.Types.InstanceMetadataProtocolState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata options for the instance.
--
-- /See:/ 'newInstanceMetadataOptionsResponse' smart constructor.
data InstanceMetadataOptionsResponse = InstanceMetadataOptionsResponse'
  { -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is
    -- not ready to process metadata traffic with the new selection.
    --
    -- @applied@ - The metadata options have been successfully applied on the
    -- instance.
    state :: Prelude.Maybe InstanceMetadataOptionsState,
    -- | Indicates whether the IPv6 endpoint for the instance metadata service is
    -- enabled or disabled.
    httpProtocolIpv6 :: Prelude.Maybe InstanceMetadataProtocolState,
    -- | Indicates whether the HTTP metadata endpoint on your instances is
    -- enabled or disabled.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
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
    -- role credential always returns the version 2.0 credentials; the version
    -- 1.0 credentials are not available.
    httpTokens :: Prelude.Maybe HttpTokensState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'instanceMetadataOptionsResponse_state' - The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
--
-- 'httpProtocolIpv6', 'instanceMetadataOptionsResponse_httpProtocolIpv6' - Indicates whether the IPv6 endpoint for the instance metadata service is
-- enabled or disabled.
--
-- 'httpEndpoint', 'instanceMetadataOptionsResponse_httpEndpoint' - Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
--
-- 'httpPutResponseHopLimit', 'instanceMetadataOptionsResponse_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'instanceMetadataOptionsResponse_httpTokens' - The state of token usage for your instance metadata requests. If the
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
-- role credential always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
newInstanceMetadataOptionsResponse ::
  InstanceMetadataOptionsResponse
newInstanceMetadataOptionsResponse =
  InstanceMetadataOptionsResponse'
    { state =
        Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      httpEndpoint = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing
    }

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
instanceMetadataOptionsResponse_state :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataOptionsState)
instanceMetadataOptionsResponse_state = Lens.lens (\InstanceMetadataOptionsResponse' {state} -> state) (\s@InstanceMetadataOptionsResponse' {} a -> s {state = a} :: InstanceMetadataOptionsResponse)

-- | Indicates whether the IPv6 endpoint for the instance metadata service is
-- enabled or disabled.
instanceMetadataOptionsResponse_httpProtocolIpv6 :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataProtocolState)
instanceMetadataOptionsResponse_httpProtocolIpv6 = Lens.lens (\InstanceMetadataOptionsResponse' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpProtocolIpv6 = a} :: InstanceMetadataOptionsResponse)

-- | Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
instanceMetadataOptionsResponse_httpEndpoint :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataEndpointState)
instanceMetadataOptionsResponse_httpEndpoint = Lens.lens (\InstanceMetadataOptionsResponse' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptionsResponse)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
instanceMetadataOptionsResponse_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe Prelude.Int)
instanceMetadataOptionsResponse_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptionsResponse' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptionsResponse)

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
-- role credential always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
instanceMetadataOptionsResponse_httpTokens :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe HttpTokensState)
instanceMetadataOptionsResponse_httpTokens = Lens.lens (\InstanceMetadataOptionsResponse' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpTokens = a} :: InstanceMetadataOptionsResponse)

instance Core.FromXML InstanceMetadataOptionsResponse where
  parseXML x =
    InstanceMetadataOptionsResponse'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "httpProtocolIpv6")
      Prelude.<*> (x Core..@? "httpEndpoint")
      Prelude.<*> (x Core..@? "httpPutResponseHopLimit")
      Prelude.<*> (x Core..@? "httpTokens")

instance
  Prelude.Hashable
    InstanceMetadataOptionsResponse

instance
  Prelude.NFData
    InstanceMetadataOptionsResponse

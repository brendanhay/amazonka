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
-- Module      : Amazonka.EC2.Types.InstanceMetadataOptionsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMetadataOptionsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HttpTokensState
import Amazonka.EC2.Types.InstanceMetadataEndpointState
import Amazonka.EC2.Types.InstanceMetadataOptionsState
import Amazonka.EC2.Types.InstanceMetadataProtocolState
import Amazonka.EC2.Types.InstanceMetadataTagsState
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instance.
--
-- /See:/ 'newInstanceMetadataOptionsResponse' smart constructor.
data InstanceMetadataOptionsResponse = InstanceMetadataOptionsResponse'
  { -- | Indicates whether the HTTP metadata endpoint on your instances is
    -- enabled or disabled.
    --
    -- If the value is @disabled@, you cannot access your instance metadata.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | Indicates whether the IPv6 endpoint for the instance metadata service is
    -- enabled or disabled.
    httpProtocolIpv6 :: Prelude.Maybe InstanceMetadataProtocolState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests.
    --
    -- If the state is @optional@, you can choose to retrieve instance metadata
    -- with or without a session token on your request. If you retrieve the IAM
    -- role credentials without a token, the version 1.0 role credentials are
    -- returned. If you retrieve the IAM role credentials using a valid session
    -- token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a session token with any
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credentials always returns the version 2.0 credentials; the version
    -- 1.0 credentials are not available.
    --
    -- Default: @optional@
    httpTokens :: Prelude.Maybe HttpTokensState,
    -- | Indicates whether access to instance tags from the instance metadata is
    -- enabled or disabled. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
    instanceMetadataTags :: Prelude.Maybe InstanceMetadataTagsState,
    -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is
    -- not ready to process metadata traffic with the new selection.
    --
    -- @applied@ - The metadata options have been successfully applied on the
    -- instance.
    state :: Prelude.Maybe InstanceMetadataOptionsState
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
-- 'httpEndpoint', 'instanceMetadataOptionsResponse_httpEndpoint' - Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
--
-- If the value is @disabled@, you cannot access your instance metadata.
--
-- 'httpProtocolIpv6', 'instanceMetadataOptionsResponse_httpProtocolIpv6' - Indicates whether the IPv6 endpoint for the instance metadata service is
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
-- 'httpTokens', 'instanceMetadataOptionsResponse_httpTokens' - The state of token usage for your instance metadata requests.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a session token on your request. If you retrieve the IAM
-- role credentials without a token, the version 1.0 role credentials are
-- returned. If you retrieve the IAM role credentials using a valid session
-- token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a session token with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
--
-- Default: @optional@
--
-- 'instanceMetadataTags', 'instanceMetadataOptionsResponse_instanceMetadataTags' - Indicates whether access to instance tags from the instance metadata is
-- enabled or disabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- 'state', 'instanceMetadataOptionsResponse_state' - The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
newInstanceMetadataOptionsResponse ::
  InstanceMetadataOptionsResponse
newInstanceMetadataOptionsResponse =
  InstanceMetadataOptionsResponse'
    { httpEndpoint =
        Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      instanceMetadataTags = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
--
-- If the value is @disabled@, you cannot access your instance metadata.
instanceMetadataOptionsResponse_httpEndpoint :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataEndpointState)
instanceMetadataOptionsResponse_httpEndpoint = Lens.lens (\InstanceMetadataOptionsResponse' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptionsResponse)

-- | Indicates whether the IPv6 endpoint for the instance metadata service is
-- enabled or disabled.
instanceMetadataOptionsResponse_httpProtocolIpv6 :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataProtocolState)
instanceMetadataOptionsResponse_httpProtocolIpv6 = Lens.lens (\InstanceMetadataOptionsResponse' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpProtocolIpv6 = a} :: InstanceMetadataOptionsResponse)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
instanceMetadataOptionsResponse_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe Prelude.Int)
instanceMetadataOptionsResponse_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptionsResponse' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptionsResponse)

-- | The state of token usage for your instance metadata requests.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a session token on your request. If you retrieve the IAM
-- role credentials without a token, the version 1.0 role credentials are
-- returned. If you retrieve the IAM role credentials using a valid session
-- token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a session token with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
--
-- Default: @optional@
instanceMetadataOptionsResponse_httpTokens :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe HttpTokensState)
instanceMetadataOptionsResponse_httpTokens = Lens.lens (\InstanceMetadataOptionsResponse' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptionsResponse' {} a -> s {httpTokens = a} :: InstanceMetadataOptionsResponse)

-- | Indicates whether access to instance tags from the instance metadata is
-- enabled or disabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
instanceMetadataOptionsResponse_instanceMetadataTags :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataTagsState)
instanceMetadataOptionsResponse_instanceMetadataTags = Lens.lens (\InstanceMetadataOptionsResponse' {instanceMetadataTags} -> instanceMetadataTags) (\s@InstanceMetadataOptionsResponse' {} a -> s {instanceMetadataTags = a} :: InstanceMetadataOptionsResponse)

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
instanceMetadataOptionsResponse_state :: Lens.Lens' InstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataOptionsState)
instanceMetadataOptionsResponse_state = Lens.lens (\InstanceMetadataOptionsResponse' {state} -> state) (\s@InstanceMetadataOptionsResponse' {} a -> s {state = a} :: InstanceMetadataOptionsResponse)

instance Data.FromXML InstanceMetadataOptionsResponse where
  parseXML x =
    InstanceMetadataOptionsResponse'
      Prelude.<$> (x Data..@? "httpEndpoint")
      Prelude.<*> (x Data..@? "httpProtocolIpv6")
      Prelude.<*> (x Data..@? "httpPutResponseHopLimit")
      Prelude.<*> (x Data..@? "httpTokens")
      Prelude.<*> (x Data..@? "instanceMetadataTags")
      Prelude.<*> (x Data..@? "state")

instance
  Prelude.Hashable
    InstanceMetadataOptionsResponse
  where
  hashWithSalt
    _salt
    InstanceMetadataOptionsResponse' {..} =
      _salt `Prelude.hashWithSalt` httpEndpoint
        `Prelude.hashWithSalt` httpProtocolIpv6
        `Prelude.hashWithSalt` httpPutResponseHopLimit
        `Prelude.hashWithSalt` httpTokens
        `Prelude.hashWithSalt` instanceMetadataTags
        `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    InstanceMetadataOptionsResponse
  where
  rnf InstanceMetadataOptionsResponse' {..} =
    Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf instanceMetadataTags
      `Prelude.seq` Prelude.rnf state

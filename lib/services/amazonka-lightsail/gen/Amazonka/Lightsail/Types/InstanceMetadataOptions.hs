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
-- Module      : Amazonka.Lightsail.Types.InstanceMetadataOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceMetadataOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.HttpEndpoint
import Amazonka.Lightsail.Types.HttpProtocolIpv6
import Amazonka.Lightsail.Types.HttpTokens
import Amazonka.Lightsail.Types.InstanceMetadataState
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instance.
--
-- /See:/ 'newInstanceMetadataOptions' smart constructor.
data InstanceMetadataOptions = InstanceMetadataOptions'
  { -- | Indicates whether the HTTP metadata endpoint on your instances is
    -- enabled or disabled.
    --
    -- If the value is @disabled@, you cannot access your instance metadata.
    httpEndpoint :: Prelude.Maybe HttpEndpoint,
    -- | Indicates whether the IPv6 endpoint for the instance metadata service is
    -- enabled or disabled.
    httpProtocolIpv6 :: Prelude.Maybe HttpProtocolIpv6,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- A larger number means that the instance metadata requests can travel
    -- farther.
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests.
    --
    -- If the state is @optional@, you can choose whether to retrieve instance
    -- metadata with a signed token header on your request. If you retrieve the
    -- IAM role credentials without a token, the version 1.0 role credentials
    -- are returned. If you retrieve the IAM role credentials by using a valid
    -- signed token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a signed token header with all
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credential always returns the version 2.0 credentials. The version
    -- 1.0 credentials are not available.
    --
    -- Not all instance blueprints in Lightsail support version 2.0
    -- credentials. Use the @MetadataNoToken@ instance metric to track the
    -- number of calls to the instance metadata service that are using version
    -- 1.0 credentials. For more information, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-viewing-instance-health-metrics Viewing instance metrics in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    httpTokens :: Prelude.Maybe HttpTokens,
    -- | The state of the metadata option changes.
    --
    -- The following states are possible:
    --
    -- -   @pending@ - The metadata options are being updated. The instance is
    --     not yet ready to process metadata traffic with the new selection.
    --
    -- -   @applied@ - The metadata options have been successfully applied to
    --     the instance.
    state :: Prelude.Maybe InstanceMetadataState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'instanceMetadataOptions_httpEndpoint' - Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
--
-- If the value is @disabled@, you cannot access your instance metadata.
--
-- 'httpProtocolIpv6', 'instanceMetadataOptions_httpProtocolIpv6' - Indicates whether the IPv6 endpoint for the instance metadata service is
-- enabled or disabled.
--
-- 'httpPutResponseHopLimit', 'instanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- A larger number means that the instance metadata requests can travel
-- farther.
--
-- 'httpTokens', 'instanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests.
--
-- If the state is @optional@, you can choose whether to retrieve instance
-- metadata with a signed token header on your request. If you retrieve the
-- IAM role credentials without a token, the version 1.0 role credentials
-- are returned. If you retrieve the IAM role credentials by using a valid
-- signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with all
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials. The version
-- 1.0 credentials are not available.
--
-- Not all instance blueprints in Lightsail support version 2.0
-- credentials. Use the @MetadataNoToken@ instance metric to track the
-- number of calls to the instance metadata service that are using version
-- 1.0 credentials. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-viewing-instance-health-metrics Viewing instance metrics in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'state', 'instanceMetadataOptions_state' - The state of the metadata option changes.
--
-- The following states are possible:
--
-- -   @pending@ - The metadata options are being updated. The instance is
--     not yet ready to process metadata traffic with the new selection.
--
-- -   @applied@ - The metadata options have been successfully applied to
--     the instance.
newInstanceMetadataOptions ::
  InstanceMetadataOptions
newInstanceMetadataOptions =
  InstanceMetadataOptions'
    { httpEndpoint =
        Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Indicates whether the HTTP metadata endpoint on your instances is
-- enabled or disabled.
--
-- If the value is @disabled@, you cannot access your instance metadata.
instanceMetadataOptions_httpEndpoint :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe HttpEndpoint)
instanceMetadataOptions_httpEndpoint = Lens.lens (\InstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptions)

-- | Indicates whether the IPv6 endpoint for the instance metadata service is
-- enabled or disabled.
instanceMetadataOptions_httpProtocolIpv6 :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe HttpProtocolIpv6)
instanceMetadataOptions_httpProtocolIpv6 = Lens.lens (\InstanceMetadataOptions' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@InstanceMetadataOptions' {} a -> s {httpProtocolIpv6 = a} :: InstanceMetadataOptions)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- A larger number means that the instance metadata requests can travel
-- farther.
instanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe Prelude.Int)
instanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptions)

-- | The state of token usage for your instance metadata requests.
--
-- If the state is @optional@, you can choose whether to retrieve instance
-- metadata with a signed token header on your request. If you retrieve the
-- IAM role credentials without a token, the version 1.0 role credentials
-- are returned. If you retrieve the IAM role credentials by using a valid
-- signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with all
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials. The version
-- 1.0 credentials are not available.
--
-- Not all instance blueprints in Lightsail support version 2.0
-- credentials. Use the @MetadataNoToken@ instance metric to track the
-- number of calls to the instance metadata service that are using version
-- 1.0 credentials. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-viewing-instance-health-metrics Viewing instance metrics in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
instanceMetadataOptions_httpTokens :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe HttpTokens)
instanceMetadataOptions_httpTokens = Lens.lens (\InstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptions' {} a -> s {httpTokens = a} :: InstanceMetadataOptions)

-- | The state of the metadata option changes.
--
-- The following states are possible:
--
-- -   @pending@ - The metadata options are being updated. The instance is
--     not yet ready to process metadata traffic with the new selection.
--
-- -   @applied@ - The metadata options have been successfully applied to
--     the instance.
instanceMetadataOptions_state :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe InstanceMetadataState)
instanceMetadataOptions_state = Lens.lens (\InstanceMetadataOptions' {state} -> state) (\s@InstanceMetadataOptions' {} a -> s {state = a} :: InstanceMetadataOptions)

instance Data.FromJSON InstanceMetadataOptions where
  parseJSON =
    Data.withObject
      "InstanceMetadataOptions"
      ( \x ->
          InstanceMetadataOptions'
            Prelude.<$> (x Data..:? "httpEndpoint")
            Prelude.<*> (x Data..:? "httpProtocolIpv6")
            Prelude.<*> (x Data..:? "httpPutResponseHopLimit")
            Prelude.<*> (x Data..:? "httpTokens")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable InstanceMetadataOptions where
  hashWithSalt _salt InstanceMetadataOptions' {..} =
    _salt
      `Prelude.hashWithSalt` httpEndpoint
      `Prelude.hashWithSalt` httpProtocolIpv6
      `Prelude.hashWithSalt` httpPutResponseHopLimit
      `Prelude.hashWithSalt` httpTokens
      `Prelude.hashWithSalt` state

instance Prelude.NFData InstanceMetadataOptions where
  rnf InstanceMetadataOptions' {..} =
    Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf state

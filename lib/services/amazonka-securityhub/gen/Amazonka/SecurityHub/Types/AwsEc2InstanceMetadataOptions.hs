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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2InstanceMetadataOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2InstanceMetadataOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata options that allow you to configure and secure the Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2InstanceMetadataOptions' smart constructor.
data AwsEc2InstanceMetadataOptions = AwsEc2InstanceMetadataOptions'
  { -- | Enables or disables the HTTP metadata endpoint on the instance.
    httpEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    httpProtocolIpv6 :: Prelude.Maybe Prelude.Text,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests.
    httpTokens :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to allow access to instance tags from the instance
    -- metadata.
    instanceMetadataTags :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2InstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'awsEc2InstanceMetadataOptions_httpEndpoint' - Enables or disables the HTTP metadata endpoint on the instance.
--
-- 'httpProtocolIpv6', 'awsEc2InstanceMetadataOptions_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
--
-- 'httpPutResponseHopLimit', 'awsEc2InstanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- 'httpTokens', 'awsEc2InstanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests.
--
-- 'instanceMetadataTags', 'awsEc2InstanceMetadataOptions_instanceMetadataTags' - Specifies whether to allow access to instance tags from the instance
-- metadata.
newAwsEc2InstanceMetadataOptions ::
  AwsEc2InstanceMetadataOptions
newAwsEc2InstanceMetadataOptions =
  AwsEc2InstanceMetadataOptions'
    { httpEndpoint =
        Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      instanceMetadataTags = Prelude.Nothing
    }

-- | Enables or disables the HTTP metadata endpoint on the instance.
awsEc2InstanceMetadataOptions_httpEndpoint :: Lens.Lens' AwsEc2InstanceMetadataOptions (Prelude.Maybe Prelude.Text)
awsEc2InstanceMetadataOptions_httpEndpoint = Lens.lens (\AwsEc2InstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@AwsEc2InstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: AwsEc2InstanceMetadataOptions)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
awsEc2InstanceMetadataOptions_httpProtocolIpv6 :: Lens.Lens' AwsEc2InstanceMetadataOptions (Prelude.Maybe Prelude.Text)
awsEc2InstanceMetadataOptions_httpProtocolIpv6 = Lens.lens (\AwsEc2InstanceMetadataOptions' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@AwsEc2InstanceMetadataOptions' {} a -> s {httpProtocolIpv6 = a} :: AwsEc2InstanceMetadataOptions)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
awsEc2InstanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' AwsEc2InstanceMetadataOptions (Prelude.Maybe Prelude.Int)
awsEc2InstanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\AwsEc2InstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@AwsEc2InstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: AwsEc2InstanceMetadataOptions)

-- | The state of token usage for your instance metadata requests.
awsEc2InstanceMetadataOptions_httpTokens :: Lens.Lens' AwsEc2InstanceMetadataOptions (Prelude.Maybe Prelude.Text)
awsEc2InstanceMetadataOptions_httpTokens = Lens.lens (\AwsEc2InstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@AwsEc2InstanceMetadataOptions' {} a -> s {httpTokens = a} :: AwsEc2InstanceMetadataOptions)

-- | Specifies whether to allow access to instance tags from the instance
-- metadata.
awsEc2InstanceMetadataOptions_instanceMetadataTags :: Lens.Lens' AwsEc2InstanceMetadataOptions (Prelude.Maybe Prelude.Text)
awsEc2InstanceMetadataOptions_instanceMetadataTags = Lens.lens (\AwsEc2InstanceMetadataOptions' {instanceMetadataTags} -> instanceMetadataTags) (\s@AwsEc2InstanceMetadataOptions' {} a -> s {instanceMetadataTags = a} :: AwsEc2InstanceMetadataOptions)

instance Data.FromJSON AwsEc2InstanceMetadataOptions where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceMetadataOptions"
      ( \x ->
          AwsEc2InstanceMetadataOptions'
            Prelude.<$> (x Data..:? "HttpEndpoint")
            Prelude.<*> (x Data..:? "HttpProtocolIpv6")
            Prelude.<*> (x Data..:? "HttpPutResponseHopLimit")
            Prelude.<*> (x Data..:? "HttpTokens")
            Prelude.<*> (x Data..:? "InstanceMetadataTags")
      )

instance
  Prelude.Hashable
    AwsEc2InstanceMetadataOptions
  where
  hashWithSalt _salt AwsEc2InstanceMetadataOptions' {..} =
    _salt
      `Prelude.hashWithSalt` httpEndpoint
      `Prelude.hashWithSalt` httpProtocolIpv6
      `Prelude.hashWithSalt` httpPutResponseHopLimit
      `Prelude.hashWithSalt` httpTokens
      `Prelude.hashWithSalt` instanceMetadataTags

instance Prelude.NFData AwsEc2InstanceMetadataOptions where
  rnf AwsEc2InstanceMetadataOptions' {..} =
    Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf instanceMetadataTags

instance Data.ToJSON AwsEc2InstanceMetadataOptions where
  toJSON AwsEc2InstanceMetadataOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HttpEndpoint" Data..=) Prelude.<$> httpEndpoint,
            ("HttpProtocolIpv6" Data..=)
              Prelude.<$> httpProtocolIpv6,
            ("HttpPutResponseHopLimit" Data..=)
              Prelude.<$> httpPutResponseHopLimit,
            ("HttpTokens" Data..=) Prelude.<$> httpTokens,
            ("InstanceMetadataTags" Data..=)
              Prelude.<$> instanceMetadataTags
          ]
      )

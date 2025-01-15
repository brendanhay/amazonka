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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMetadataOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMetadataOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the metadata options for an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataMetadataOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataMetadataOptionsDetails = AwsEc2LaunchTemplateDataMetadataOptionsDetails'
  { -- | Enables or disables the HTTP metadata endpoint on your instances. If the
    -- parameter is not specified, the default state is enabled, and you won’t
    -- be able to access your instance metadata.
    httpEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    httpProtocolIpv6 :: Prelude.Maybe Prelude.Text,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests.
    httpTokens :: Prelude.Maybe Prelude.Text,
    -- | When set to @enabled@, this parameter allows access to instance tags
    -- from the instance metadata. When set to @disabled@, it turns off access
    -- to instance tags from the instance metadata. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags in instance metadata>
    -- in the /Amazon EC2 User Guide/.
    instanceMetadataTags :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataMetadataOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'awsEc2LaunchTemplateDataMetadataOptionsDetails_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances. If the
-- parameter is not specified, the default state is enabled, and you won’t
-- be able to access your instance metadata.
--
-- 'httpProtocolIpv6', 'awsEc2LaunchTemplateDataMetadataOptionsDetails_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
--
-- 'httpPutResponseHopLimit', 'awsEc2LaunchTemplateDataMetadataOptionsDetails_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- 'httpTokens', 'awsEc2LaunchTemplateDataMetadataOptionsDetails_httpTokens' - The state of token usage for your instance metadata requests.
--
-- 'instanceMetadataTags', 'awsEc2LaunchTemplateDataMetadataOptionsDetails_instanceMetadataTags' - When set to @enabled@, this parameter allows access to instance tags
-- from the instance metadata. When set to @disabled@, it turns off access
-- to instance tags from the instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags in instance metadata>
-- in the /Amazon EC2 User Guide/.
newAwsEc2LaunchTemplateDataMetadataOptionsDetails ::
  AwsEc2LaunchTemplateDataMetadataOptionsDetails
newAwsEc2LaunchTemplateDataMetadataOptionsDetails =
  AwsEc2LaunchTemplateDataMetadataOptionsDetails'
    { httpEndpoint =
        Prelude.Nothing,
      httpProtocolIpv6 =
        Prelude.Nothing,
      httpPutResponseHopLimit =
        Prelude.Nothing,
      httpTokens =
        Prelude.Nothing,
      instanceMetadataTags =
        Prelude.Nothing
    }

-- | Enables or disables the HTTP metadata endpoint on your instances. If the
-- parameter is not specified, the default state is enabled, and you won’t
-- be able to access your instance metadata.
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpEndpoint :: Lens.Lens' AwsEc2LaunchTemplateDataMetadataOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpEndpoint = Lens.lens (\AwsEc2LaunchTemplateDataMetadataOptionsDetails' {httpEndpoint} -> httpEndpoint) (\s@AwsEc2LaunchTemplateDataMetadataOptionsDetails' {} a -> s {httpEndpoint = a} :: AwsEc2LaunchTemplateDataMetadataOptionsDetails)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpProtocolIpv6 :: Lens.Lens' AwsEc2LaunchTemplateDataMetadataOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpProtocolIpv6 = Lens.lens (\AwsEc2LaunchTemplateDataMetadataOptionsDetails' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@AwsEc2LaunchTemplateDataMetadataOptionsDetails' {} a -> s {httpProtocolIpv6 = a} :: AwsEc2LaunchTemplateDataMetadataOptionsDetails)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpPutResponseHopLimit :: Lens.Lens' AwsEc2LaunchTemplateDataMetadataOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpPutResponseHopLimit = Lens.lens (\AwsEc2LaunchTemplateDataMetadataOptionsDetails' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@AwsEc2LaunchTemplateDataMetadataOptionsDetails' {} a -> s {httpPutResponseHopLimit = a} :: AwsEc2LaunchTemplateDataMetadataOptionsDetails)

-- | The state of token usage for your instance metadata requests.
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpTokens :: Lens.Lens' AwsEc2LaunchTemplateDataMetadataOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataMetadataOptionsDetails_httpTokens = Lens.lens (\AwsEc2LaunchTemplateDataMetadataOptionsDetails' {httpTokens} -> httpTokens) (\s@AwsEc2LaunchTemplateDataMetadataOptionsDetails' {} a -> s {httpTokens = a} :: AwsEc2LaunchTemplateDataMetadataOptionsDetails)

-- | When set to @enabled@, this parameter allows access to instance tags
-- from the instance metadata. When set to @disabled@, it turns off access
-- to instance tags from the instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags in instance metadata>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataMetadataOptionsDetails_instanceMetadataTags :: Lens.Lens' AwsEc2LaunchTemplateDataMetadataOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataMetadataOptionsDetails_instanceMetadataTags = Lens.lens (\AwsEc2LaunchTemplateDataMetadataOptionsDetails' {instanceMetadataTags} -> instanceMetadataTags) (\s@AwsEc2LaunchTemplateDataMetadataOptionsDetails' {} a -> s {instanceMetadataTags = a} :: AwsEc2LaunchTemplateDataMetadataOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataMetadataOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataMetadataOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataMetadataOptionsDetails'
            Prelude.<$> (x Data..:? "HttpEndpoint")
            Prelude.<*> (x Data..:? "HttpProtocolIpv6")
            Prelude.<*> (x Data..:? "HttpPutResponseHopLimit")
            Prelude.<*> (x Data..:? "HttpTokens")
            Prelude.<*> (x Data..:? "InstanceMetadataTags")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataMetadataOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataMetadataOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` httpEndpoint
        `Prelude.hashWithSalt` httpProtocolIpv6
        `Prelude.hashWithSalt` httpPutResponseHopLimit
        `Prelude.hashWithSalt` httpTokens
        `Prelude.hashWithSalt` instanceMetadataTags

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataMetadataOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataMetadataOptionsDetails' {..} =
      Prelude.rnf httpEndpoint `Prelude.seq`
        Prelude.rnf httpProtocolIpv6 `Prelude.seq`
          Prelude.rnf httpPutResponseHopLimit `Prelude.seq`
            Prelude.rnf httpTokens `Prelude.seq`
              Prelude.rnf instanceMetadataTags

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataMetadataOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataMetadataOptionsDetails' {..} =
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

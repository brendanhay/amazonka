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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationMetadataOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationMetadataOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instances.
--
-- /See:/ 'newAwsAutoScalingLaunchConfigurationMetadataOptions' smart constructor.
data AwsAutoScalingLaunchConfigurationMetadataOptions = AwsAutoScalingLaunchConfigurationMetadataOptions'
  { -- | Enables or disables the HTTP metadata endpoint on your instances. By
    -- default, the metadata endpoint is enabled.
    httpEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The HTTP @PUT@ response hop limit for instance metadata requests. The
    -- larger the number, the further instance metadata requests can travel.
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether token usage is @required@ or @optional@ for metadata
    -- requests. By default, token usage is @optional@.
    httpTokens :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingLaunchConfigurationMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances. By
-- default, the metadata endpoint is enabled.
--
-- 'httpPutResponseHopLimit', 'awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit' - The HTTP @PUT@ response hop limit for instance metadata requests. The
-- larger the number, the further instance metadata requests can travel.
--
-- 'httpTokens', 'awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens' - Indicates whether token usage is @required@ or @optional@ for metadata
-- requests. By default, token usage is @optional@.
newAwsAutoScalingLaunchConfigurationMetadataOptions ::
  AwsAutoScalingLaunchConfigurationMetadataOptions
newAwsAutoScalingLaunchConfigurationMetadataOptions =
  AwsAutoScalingLaunchConfigurationMetadataOptions'
    { httpEndpoint =
        Prelude.Nothing,
      httpPutResponseHopLimit =
        Prelude.Nothing,
      httpTokens =
        Prelude.Nothing
    }

-- | Enables or disables the HTTP metadata endpoint on your instances. By
-- default, the metadata endpoint is enabled.
awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint :: Lens.Lens' AwsAutoScalingLaunchConfigurationMetadataOptions (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint = Lens.lens (\AwsAutoScalingLaunchConfigurationMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@AwsAutoScalingLaunchConfigurationMetadataOptions' {} a -> s {httpEndpoint = a} :: AwsAutoScalingLaunchConfigurationMetadataOptions)

-- | The HTTP @PUT@ response hop limit for instance metadata requests. The
-- larger the number, the further instance metadata requests can travel.
awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' AwsAutoScalingLaunchConfigurationMetadataOptions (Prelude.Maybe Prelude.Int)
awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit = Lens.lens (\AwsAutoScalingLaunchConfigurationMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@AwsAutoScalingLaunchConfigurationMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: AwsAutoScalingLaunchConfigurationMetadataOptions)

-- | Indicates whether token usage is @required@ or @optional@ for metadata
-- requests. By default, token usage is @optional@.
awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens :: Lens.Lens' AwsAutoScalingLaunchConfigurationMetadataOptions (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens = Lens.lens (\AwsAutoScalingLaunchConfigurationMetadataOptions' {httpTokens} -> httpTokens) (\s@AwsAutoScalingLaunchConfigurationMetadataOptions' {} a -> s {httpTokens = a} :: AwsAutoScalingLaunchConfigurationMetadataOptions)

instance
  Data.FromJSON
    AwsAutoScalingLaunchConfigurationMetadataOptions
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingLaunchConfigurationMetadataOptions"
      ( \x ->
          AwsAutoScalingLaunchConfigurationMetadataOptions'
            Prelude.<$> (x Data..:? "HttpEndpoint")
            Prelude.<*> (x Data..:? "HttpPutResponseHopLimit")
            Prelude.<*> (x Data..:? "HttpTokens")
      )

instance
  Prelude.Hashable
    AwsAutoScalingLaunchConfigurationMetadataOptions
  where
  hashWithSalt
    _salt
    AwsAutoScalingLaunchConfigurationMetadataOptions' {..} =
      _salt
        `Prelude.hashWithSalt` httpEndpoint
        `Prelude.hashWithSalt` httpPutResponseHopLimit
        `Prelude.hashWithSalt` httpTokens

instance
  Prelude.NFData
    AwsAutoScalingLaunchConfigurationMetadataOptions
  where
  rnf
    AwsAutoScalingLaunchConfigurationMetadataOptions' {..} =
      Prelude.rnf httpEndpoint
        `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
        `Prelude.seq` Prelude.rnf httpTokens

instance
  Data.ToJSON
    AwsAutoScalingLaunchConfigurationMetadataOptions
  where
  toJSON
    AwsAutoScalingLaunchConfigurationMetadataOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("HttpEndpoint" Data..=) Prelude.<$> httpEndpoint,
              ("HttpPutResponseHopLimit" Data..=)
                Prelude.<$> httpPutResponseHopLimit,
              ("HttpTokens" Data..=) Prelude.<$> httpTokens
            ]
        )

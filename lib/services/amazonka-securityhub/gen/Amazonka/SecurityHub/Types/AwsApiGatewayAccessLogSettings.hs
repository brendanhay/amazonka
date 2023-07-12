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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about settings for logging access for the stage.
--
-- /See:/ 'newAwsApiGatewayAccessLogSettings' smart constructor.
data AwsApiGatewayAccessLogSettings = AwsApiGatewayAccessLogSettings'
  { -- | The ARN of the CloudWatch Logs log group that receives the access logs.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | A single-line format of the access logs of data, as specified by
    -- selected @$context@ variables. The format must include at least
    -- @$context.requestId@.
    format :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayAccessLogSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'awsApiGatewayAccessLogSettings_destinationArn' - The ARN of the CloudWatch Logs log group that receives the access logs.
--
-- 'format', 'awsApiGatewayAccessLogSettings_format' - A single-line format of the access logs of data, as specified by
-- selected @$context@ variables. The format must include at least
-- @$context.requestId@.
newAwsApiGatewayAccessLogSettings ::
  AwsApiGatewayAccessLogSettings
newAwsApiGatewayAccessLogSettings =
  AwsApiGatewayAccessLogSettings'
    { destinationArn =
        Prelude.Nothing,
      format = Prelude.Nothing
    }

-- | The ARN of the CloudWatch Logs log group that receives the access logs.
awsApiGatewayAccessLogSettings_destinationArn :: Lens.Lens' AwsApiGatewayAccessLogSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayAccessLogSettings_destinationArn = Lens.lens (\AwsApiGatewayAccessLogSettings' {destinationArn} -> destinationArn) (\s@AwsApiGatewayAccessLogSettings' {} a -> s {destinationArn = a} :: AwsApiGatewayAccessLogSettings)

-- | A single-line format of the access logs of data, as specified by
-- selected @$context@ variables. The format must include at least
-- @$context.requestId@.
awsApiGatewayAccessLogSettings_format :: Lens.Lens' AwsApiGatewayAccessLogSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayAccessLogSettings_format = Lens.lens (\AwsApiGatewayAccessLogSettings' {format} -> format) (\s@AwsApiGatewayAccessLogSettings' {} a -> s {format = a} :: AwsApiGatewayAccessLogSettings)

instance Data.FromJSON AwsApiGatewayAccessLogSettings where
  parseJSON =
    Data.withObject
      "AwsApiGatewayAccessLogSettings"
      ( \x ->
          AwsApiGatewayAccessLogSettings'
            Prelude.<$> (x Data..:? "DestinationArn")
            Prelude.<*> (x Data..:? "Format")
      )

instance
  Prelude.Hashable
    AwsApiGatewayAccessLogSettings
  where
  hashWithSalt
    _salt
    AwsApiGatewayAccessLogSettings' {..} =
      _salt
        `Prelude.hashWithSalt` destinationArn
        `Prelude.hashWithSalt` format

instance
  Prelude.NFData
    AwsApiGatewayAccessLogSettings
  where
  rnf AwsApiGatewayAccessLogSettings' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON AwsApiGatewayAccessLogSettings where
  toJSON AwsApiGatewayAccessLogSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationArn" Data..=)
              Prelude.<$> destinationArn,
            ("Format" Data..=) Prelude.<$> format
          ]
      )

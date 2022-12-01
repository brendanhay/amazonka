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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about settings for logging access for the stage.
--
-- /See:/ 'newAwsApiGatewayAccessLogSettings' smart constructor.
data AwsApiGatewayAccessLogSettings = AwsApiGatewayAccessLogSettings'
  { -- | A single-line format of the access logs of data, as specified by
    -- selected @$context@ variables. The format must include at least
    -- @$context.requestId@.
    format :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the CloudWatch Logs log group that receives the access logs.
    destinationArn :: Prelude.Maybe Prelude.Text
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
-- 'format', 'awsApiGatewayAccessLogSettings_format' - A single-line format of the access logs of data, as specified by
-- selected @$context@ variables. The format must include at least
-- @$context.requestId@.
--
-- 'destinationArn', 'awsApiGatewayAccessLogSettings_destinationArn' - The ARN of the CloudWatch Logs log group that receives the access logs.
newAwsApiGatewayAccessLogSettings ::
  AwsApiGatewayAccessLogSettings
newAwsApiGatewayAccessLogSettings =
  AwsApiGatewayAccessLogSettings'
    { format =
        Prelude.Nothing,
      destinationArn = Prelude.Nothing
    }

-- | A single-line format of the access logs of data, as specified by
-- selected @$context@ variables. The format must include at least
-- @$context.requestId@.
awsApiGatewayAccessLogSettings_format :: Lens.Lens' AwsApiGatewayAccessLogSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayAccessLogSettings_format = Lens.lens (\AwsApiGatewayAccessLogSettings' {format} -> format) (\s@AwsApiGatewayAccessLogSettings' {} a -> s {format = a} :: AwsApiGatewayAccessLogSettings)

-- | The ARN of the CloudWatch Logs log group that receives the access logs.
awsApiGatewayAccessLogSettings_destinationArn :: Lens.Lens' AwsApiGatewayAccessLogSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayAccessLogSettings_destinationArn = Lens.lens (\AwsApiGatewayAccessLogSettings' {destinationArn} -> destinationArn) (\s@AwsApiGatewayAccessLogSettings' {} a -> s {destinationArn = a} :: AwsApiGatewayAccessLogSettings)

instance Core.FromJSON AwsApiGatewayAccessLogSettings where
  parseJSON =
    Core.withObject
      "AwsApiGatewayAccessLogSettings"
      ( \x ->
          AwsApiGatewayAccessLogSettings'
            Prelude.<$> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "DestinationArn")
      )

instance
  Prelude.Hashable
    AwsApiGatewayAccessLogSettings
  where
  hashWithSalt
    _salt
    AwsApiGatewayAccessLogSettings' {..} =
      _salt `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` destinationArn

instance
  Prelude.NFData
    AwsApiGatewayAccessLogSettings
  where
  rnf AwsApiGatewayAccessLogSettings' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf destinationArn

instance Core.ToJSON AwsApiGatewayAccessLogSettings where
  toJSON AwsApiGatewayAccessLogSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Format" Core..=) Prelude.<$> format,
            ("DestinationArn" Core..=)
              Prelude.<$> destinationArn
          ]
      )

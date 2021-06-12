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
-- Module      : Network.AWS.APIGateway.Types.AccessLogSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AccessLogSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Access log settings, including the access log format and access log
-- destination ARN.
--
-- /See:/ 'newAccessLogSettings' smart constructor.
data AccessLogSettings = AccessLogSettings'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or
    -- Kinesis Data Firehose delivery stream to receive access logs. If you
    -- specify a Kinesis Data Firehose delivery stream, the stream name must
    -- begin with @amazon-apigateway-@.
    destinationArn :: Core.Maybe Core.Text,
    -- | A single line format of the access logs of data, as specified by
    -- selected
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference $context variables>.
    -- The format must include at least @$context.requestId@.
    format :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessLogSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'accessLogSettings_destinationArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or
-- Kinesis Data Firehose delivery stream to receive access logs. If you
-- specify a Kinesis Data Firehose delivery stream, the stream name must
-- begin with @amazon-apigateway-@.
--
-- 'format', 'accessLogSettings_format' - A single line format of the access logs of data, as specified by
-- selected
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference $context variables>.
-- The format must include at least @$context.requestId@.
newAccessLogSettings ::
  AccessLogSettings
newAccessLogSettings =
  AccessLogSettings'
    { destinationArn = Core.Nothing,
      format = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or
-- Kinesis Data Firehose delivery stream to receive access logs. If you
-- specify a Kinesis Data Firehose delivery stream, the stream name must
-- begin with @amazon-apigateway-@.
accessLogSettings_destinationArn :: Lens.Lens' AccessLogSettings (Core.Maybe Core.Text)
accessLogSettings_destinationArn = Lens.lens (\AccessLogSettings' {destinationArn} -> destinationArn) (\s@AccessLogSettings' {} a -> s {destinationArn = a} :: AccessLogSettings)

-- | A single line format of the access logs of data, as specified by
-- selected
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference $context variables>.
-- The format must include at least @$context.requestId@.
accessLogSettings_format :: Lens.Lens' AccessLogSettings (Core.Maybe Core.Text)
accessLogSettings_format = Lens.lens (\AccessLogSettings' {format} -> format) (\s@AccessLogSettings' {} a -> s {format = a} :: AccessLogSettings)

instance Core.FromJSON AccessLogSettings where
  parseJSON =
    Core.withObject
      "AccessLogSettings"
      ( \x ->
          AccessLogSettings'
            Core.<$> (x Core..:? "destinationArn")
            Core.<*> (x Core..:? "format")
      )

instance Core.Hashable AccessLogSettings

instance Core.NFData AccessLogSettings

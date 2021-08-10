{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateRealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a real-time log configuration.
--
-- When you update a real-time log configuration, all the parameters are
-- updated with the values provided in the request. You cannot update some
-- parameters independent of others. To update a real-time log
-- configuration:
--
-- 1.  Call @GetRealtimeLogConfig@ to get the current real-time log
--     configuration.
--
-- 2.  Locally modify the parameters in the real-time log configuration
--     that you want to update.
--
-- 3.  Call this API (@UpdateRealtimeLogConfig@) by providing the entire
--     real-time log configuration, including the parameters that you
--     modified and those that you didn’t.
--
-- You cannot update a real-time log configuration’s @Name@ or @ARN@.
module Network.AWS.CloudFront.UpdateRealtimeLogConfig
  ( -- * Creating a Request
    UpdateRealtimeLogConfig (..),
    newUpdateRealtimeLogConfig,

    -- * Request Lenses
    updateRealtimeLogConfig_samplingRate,
    updateRealtimeLogConfig_endPoints,
    updateRealtimeLogConfig_arn,
    updateRealtimeLogConfig_name,
    updateRealtimeLogConfig_fields,

    -- * Destructuring the Response
    UpdateRealtimeLogConfigResponse (..),
    newUpdateRealtimeLogConfigResponse,

    -- * Response Lenses
    updateRealtimeLogConfigResponse_realtimeLogConfig,
    updateRealtimeLogConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRealtimeLogConfig' smart constructor.
data UpdateRealtimeLogConfig = UpdateRealtimeLogConfig'
  { -- | The sampling rate for this real-time log configuration. The sampling
    -- rate determines the percentage of viewer requests that are represented
    -- in the real-time log data. You must provide an integer between 1 and
    -- 100, inclusive.
    samplingRate :: Prelude.Maybe Prelude.Integer,
    -- | Contains information about the Amazon Kinesis data stream where you are
    -- sending real-time log data.
    endPoints :: Prelude.Maybe [EndPoint],
    -- | The Amazon Resource Name (ARN) for this real-time log configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name for this real-time log configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of fields to include in each real-time log record.
    --
    -- For more information about fields, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
    -- in the /Amazon CloudFront Developer Guide/.
    fields :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRate', 'updateRealtimeLogConfig_samplingRate' - The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. You must provide an integer between 1 and
-- 100, inclusive.
--
-- 'endPoints', 'updateRealtimeLogConfig_endPoints' - Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
--
-- 'arn', 'updateRealtimeLogConfig_arn' - The Amazon Resource Name (ARN) for this real-time log configuration.
--
-- 'name', 'updateRealtimeLogConfig_name' - The name for this real-time log configuration.
--
-- 'fields', 'updateRealtimeLogConfig_fields' - A list of fields to include in each real-time log record.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
newUpdateRealtimeLogConfig ::
  UpdateRealtimeLogConfig
newUpdateRealtimeLogConfig =
  UpdateRealtimeLogConfig'
    { samplingRate =
        Prelude.Nothing,
      endPoints = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      fields = Prelude.Nothing
    }

-- | The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. You must provide an integer between 1 and
-- 100, inclusive.
updateRealtimeLogConfig_samplingRate :: Lens.Lens' UpdateRealtimeLogConfig (Prelude.Maybe Prelude.Integer)
updateRealtimeLogConfig_samplingRate = Lens.lens (\UpdateRealtimeLogConfig' {samplingRate} -> samplingRate) (\s@UpdateRealtimeLogConfig' {} a -> s {samplingRate = a} :: UpdateRealtimeLogConfig)

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
updateRealtimeLogConfig_endPoints :: Lens.Lens' UpdateRealtimeLogConfig (Prelude.Maybe [EndPoint])
updateRealtimeLogConfig_endPoints = Lens.lens (\UpdateRealtimeLogConfig' {endPoints} -> endPoints) (\s@UpdateRealtimeLogConfig' {} a -> s {endPoints = a} :: UpdateRealtimeLogConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for this real-time log configuration.
updateRealtimeLogConfig_arn :: Lens.Lens' UpdateRealtimeLogConfig (Prelude.Maybe Prelude.Text)
updateRealtimeLogConfig_arn = Lens.lens (\UpdateRealtimeLogConfig' {arn} -> arn) (\s@UpdateRealtimeLogConfig' {} a -> s {arn = a} :: UpdateRealtimeLogConfig)

-- | The name for this real-time log configuration.
updateRealtimeLogConfig_name :: Lens.Lens' UpdateRealtimeLogConfig (Prelude.Maybe Prelude.Text)
updateRealtimeLogConfig_name = Lens.lens (\UpdateRealtimeLogConfig' {name} -> name) (\s@UpdateRealtimeLogConfig' {} a -> s {name = a} :: UpdateRealtimeLogConfig)

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
updateRealtimeLogConfig_fields :: Lens.Lens' UpdateRealtimeLogConfig (Prelude.Maybe [Prelude.Text])
updateRealtimeLogConfig_fields = Lens.lens (\UpdateRealtimeLogConfig' {fields} -> fields) (\s@UpdateRealtimeLogConfig' {} a -> s {fields = a} :: UpdateRealtimeLogConfig) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest UpdateRealtimeLogConfig where
  type
    AWSResponse UpdateRealtimeLogConfig =
      UpdateRealtimeLogConfigResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateRealtimeLogConfigResponse'
            Prelude.<$> (x Core..@? "RealtimeLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRealtimeLogConfig

instance Prelude.NFData UpdateRealtimeLogConfig

instance Core.ToElement UpdateRealtimeLogConfig where
  toElement =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}UpdateRealtimeLogConfigRequest"

instance Core.ToHeaders UpdateRealtimeLogConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateRealtimeLogConfig where
  toPath =
    Prelude.const "/2020-05-31/realtime-log-config/"

instance Core.ToQuery UpdateRealtimeLogConfig where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML UpdateRealtimeLogConfig where
  toXML UpdateRealtimeLogConfig' {..} =
    Prelude.mconcat
      [ "SamplingRate" Core.@= samplingRate,
        "EndPoints"
          Core.@= Core.toXML
            (Core.toXMLList "member" Prelude.<$> endPoints),
        "ARN" Core.@= arn,
        "Name" Core.@= name,
        "Fields"
          Core.@= Core.toXML
            (Core.toXMLList "Field" Prelude.<$> fields)
      ]

-- | /See:/ 'newUpdateRealtimeLogConfigResponse' smart constructor.
data UpdateRealtimeLogConfigResponse = UpdateRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Prelude.Maybe RealtimeLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeLogConfig', 'updateRealtimeLogConfigResponse_realtimeLogConfig' - A real-time log configuration.
--
-- 'httpStatus', 'updateRealtimeLogConfigResponse_httpStatus' - The response's http status code.
newUpdateRealtimeLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRealtimeLogConfigResponse
newUpdateRealtimeLogConfigResponse pHttpStatus_ =
  UpdateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A real-time log configuration.
updateRealtimeLogConfigResponse_realtimeLogConfig :: Lens.Lens' UpdateRealtimeLogConfigResponse (Prelude.Maybe RealtimeLogConfig)
updateRealtimeLogConfigResponse_realtimeLogConfig = Lens.lens (\UpdateRealtimeLogConfigResponse' {realtimeLogConfig} -> realtimeLogConfig) (\s@UpdateRealtimeLogConfigResponse' {} a -> s {realtimeLogConfig = a} :: UpdateRealtimeLogConfigResponse)

-- | The response's http status code.
updateRealtimeLogConfigResponse_httpStatus :: Lens.Lens' UpdateRealtimeLogConfigResponse Prelude.Int
updateRealtimeLogConfigResponse_httpStatus = Lens.lens (\UpdateRealtimeLogConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateRealtimeLogConfigResponse' {} a -> s {httpStatus = a} :: UpdateRealtimeLogConfigResponse)

instance
  Prelude.NFData
    UpdateRealtimeLogConfigResponse

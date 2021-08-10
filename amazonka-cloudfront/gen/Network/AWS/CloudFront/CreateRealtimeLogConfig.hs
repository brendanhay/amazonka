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
-- Module      : Network.AWS.CloudFront.CreateRealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time log configuration.
--
-- After you create a real-time log configuration, you can attach it to one
-- or more cache behaviors to send real-time log data to the specified
-- Amazon Kinesis data stream.
--
-- For more information about real-time log configurations, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateRealtimeLogConfig
  ( -- * Creating a Request
    CreateRealtimeLogConfig (..),
    newCreateRealtimeLogConfig,

    -- * Request Lenses
    createRealtimeLogConfig_endPoints,
    createRealtimeLogConfig_fields,
    createRealtimeLogConfig_name,
    createRealtimeLogConfig_samplingRate,

    -- * Destructuring the Response
    CreateRealtimeLogConfigResponse (..),
    newCreateRealtimeLogConfigResponse,

    -- * Response Lenses
    createRealtimeLogConfigResponse_realtimeLogConfig,
    createRealtimeLogConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRealtimeLogConfig' smart constructor.
data CreateRealtimeLogConfig = CreateRealtimeLogConfig'
  { -- | Contains information about the Amazon Kinesis data stream where you are
    -- sending real-time log data.
    endPoints :: [EndPoint],
    -- | A list of fields to include in each real-time log record.
    --
    -- For more information about fields, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
    -- in the /Amazon CloudFront Developer Guide/.
    fields :: [Prelude.Text],
    -- | A unique name to identify this real-time log configuration.
    name :: Prelude.Text,
    -- | The sampling rate for this real-time log configuration. The sampling
    -- rate determines the percentage of viewer requests that are represented
    -- in the real-time log data. You must provide an integer between 1 and
    -- 100, inclusive.
    samplingRate :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endPoints', 'createRealtimeLogConfig_endPoints' - Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
--
-- 'fields', 'createRealtimeLogConfig_fields' - A list of fields to include in each real-time log record.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'name', 'createRealtimeLogConfig_name' - A unique name to identify this real-time log configuration.
--
-- 'samplingRate', 'createRealtimeLogConfig_samplingRate' - The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. You must provide an integer between 1 and
-- 100, inclusive.
newCreateRealtimeLogConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'samplingRate'
  Prelude.Integer ->
  CreateRealtimeLogConfig
newCreateRealtimeLogConfig pName_ pSamplingRate_ =
  CreateRealtimeLogConfig'
    { endPoints =
        Prelude.mempty,
      fields = Prelude.mempty,
      name = pName_,
      samplingRate = pSamplingRate_
    }

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
createRealtimeLogConfig_endPoints :: Lens.Lens' CreateRealtimeLogConfig [EndPoint]
createRealtimeLogConfig_endPoints = Lens.lens (\CreateRealtimeLogConfig' {endPoints} -> endPoints) (\s@CreateRealtimeLogConfig' {} a -> s {endPoints = a} :: CreateRealtimeLogConfig) Prelude.. Lens._Coerce

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
createRealtimeLogConfig_fields :: Lens.Lens' CreateRealtimeLogConfig [Prelude.Text]
createRealtimeLogConfig_fields = Lens.lens (\CreateRealtimeLogConfig' {fields} -> fields) (\s@CreateRealtimeLogConfig' {} a -> s {fields = a} :: CreateRealtimeLogConfig) Prelude.. Lens._Coerce

-- | A unique name to identify this real-time log configuration.
createRealtimeLogConfig_name :: Lens.Lens' CreateRealtimeLogConfig Prelude.Text
createRealtimeLogConfig_name = Lens.lens (\CreateRealtimeLogConfig' {name} -> name) (\s@CreateRealtimeLogConfig' {} a -> s {name = a} :: CreateRealtimeLogConfig)

-- | The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. You must provide an integer between 1 and
-- 100, inclusive.
createRealtimeLogConfig_samplingRate :: Lens.Lens' CreateRealtimeLogConfig Prelude.Integer
createRealtimeLogConfig_samplingRate = Lens.lens (\CreateRealtimeLogConfig' {samplingRate} -> samplingRate) (\s@CreateRealtimeLogConfig' {} a -> s {samplingRate = a} :: CreateRealtimeLogConfig)

instance Core.AWSRequest CreateRealtimeLogConfig where
  type
    AWSResponse CreateRealtimeLogConfig =
      CreateRealtimeLogConfigResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRealtimeLogConfigResponse'
            Prelude.<$> (x Core..@? "RealtimeLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRealtimeLogConfig

instance Prelude.NFData CreateRealtimeLogConfig

instance Core.ToElement CreateRealtimeLogConfig where
  toElement =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CreateRealtimeLogConfigRequest"

instance Core.ToHeaders CreateRealtimeLogConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateRealtimeLogConfig where
  toPath =
    Prelude.const "/2020-05-31/realtime-log-config"

instance Core.ToQuery CreateRealtimeLogConfig where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML CreateRealtimeLogConfig where
  toXML CreateRealtimeLogConfig' {..} =
    Prelude.mconcat
      [ "EndPoints"
          Core.@= Core.toXMLList "member" endPoints,
        "Fields" Core.@= Core.toXMLList "Field" fields,
        "Name" Core.@= name,
        "SamplingRate" Core.@= samplingRate
      ]

-- | /See:/ 'newCreateRealtimeLogConfigResponse' smart constructor.
data CreateRealtimeLogConfigResponse = CreateRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Prelude.Maybe RealtimeLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeLogConfig', 'createRealtimeLogConfigResponse_realtimeLogConfig' - A real-time log configuration.
--
-- 'httpStatus', 'createRealtimeLogConfigResponse_httpStatus' - The response's http status code.
newCreateRealtimeLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRealtimeLogConfigResponse
newCreateRealtimeLogConfigResponse pHttpStatus_ =
  CreateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A real-time log configuration.
createRealtimeLogConfigResponse_realtimeLogConfig :: Lens.Lens' CreateRealtimeLogConfigResponse (Prelude.Maybe RealtimeLogConfig)
createRealtimeLogConfigResponse_realtimeLogConfig = Lens.lens (\CreateRealtimeLogConfigResponse' {realtimeLogConfig} -> realtimeLogConfig) (\s@CreateRealtimeLogConfigResponse' {} a -> s {realtimeLogConfig = a} :: CreateRealtimeLogConfigResponse)

-- | The response's http status code.
createRealtimeLogConfigResponse_httpStatus :: Lens.Lens' CreateRealtimeLogConfigResponse Prelude.Int
createRealtimeLogConfigResponse_httpStatus = Lens.lens (\CreateRealtimeLogConfigResponse' {httpStatus} -> httpStatus) (\s@CreateRealtimeLogConfigResponse' {} a -> s {httpStatus = a} :: CreateRealtimeLogConfigResponse)

instance
  Prelude.NFData
    CreateRealtimeLogConfigResponse

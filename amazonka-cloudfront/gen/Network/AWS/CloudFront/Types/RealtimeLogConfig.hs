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
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfig where

import Network.AWS.CloudFront.Types.EndPoint
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A real-time log configuration.
--
-- /See:/ 'newRealtimeLogConfig' smart constructor.
data RealtimeLogConfig = RealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of this real-time log configuration.
    arn :: Prelude.Text,
    -- | The unique name of this real-time log configuration.
    name :: Prelude.Text,
    -- | The sampling rate for this real-time log configuration. The sampling
    -- rate determines the percentage of viewer requests that are represented
    -- in the real-time log data. The sampling rate is an integer between 1 and
    -- 100, inclusive.
    samplingRate :: Prelude.Integer,
    -- | Contains information about the Amazon Kinesis data stream where you are
    -- sending real-time log data for this real-time log configuration.
    endPoints :: [EndPoint],
    -- | A list of fields that are included in each real-time log record. In an
    -- API response, the fields are provided in the same order in which they
    -- are sent to the Amazon Kinesis data stream.
    --
    -- For more information about fields, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
    -- in the /Amazon CloudFront Developer Guide/.
    fields :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'realtimeLogConfig_arn' - The Amazon Resource Name (ARN) of this real-time log configuration.
--
-- 'name', 'realtimeLogConfig_name' - The unique name of this real-time log configuration.
--
-- 'samplingRate', 'realtimeLogConfig_samplingRate' - The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. The sampling rate is an integer between 1 and
-- 100, inclusive.
--
-- 'endPoints', 'realtimeLogConfig_endPoints' - Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data for this real-time log configuration.
--
-- 'fields', 'realtimeLogConfig_fields' - A list of fields that are included in each real-time log record. In an
-- API response, the fields are provided in the same order in which they
-- are sent to the Amazon Kinesis data stream.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
newRealtimeLogConfig ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'samplingRate'
  Prelude.Integer ->
  RealtimeLogConfig
newRealtimeLogConfig pARN_ pName_ pSamplingRate_ =
  RealtimeLogConfig'
    { arn = pARN_,
      name = pName_,
      samplingRate = pSamplingRate_,
      endPoints = Prelude.mempty,
      fields = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of this real-time log configuration.
realtimeLogConfig_arn :: Lens.Lens' RealtimeLogConfig Prelude.Text
realtimeLogConfig_arn = Lens.lens (\RealtimeLogConfig' {arn} -> arn) (\s@RealtimeLogConfig' {} a -> s {arn = a} :: RealtimeLogConfig)

-- | The unique name of this real-time log configuration.
realtimeLogConfig_name :: Lens.Lens' RealtimeLogConfig Prelude.Text
realtimeLogConfig_name = Lens.lens (\RealtimeLogConfig' {name} -> name) (\s@RealtimeLogConfig' {} a -> s {name = a} :: RealtimeLogConfig)

-- | The sampling rate for this real-time log configuration. The sampling
-- rate determines the percentage of viewer requests that are represented
-- in the real-time log data. The sampling rate is an integer between 1 and
-- 100, inclusive.
realtimeLogConfig_samplingRate :: Lens.Lens' RealtimeLogConfig Prelude.Integer
realtimeLogConfig_samplingRate = Lens.lens (\RealtimeLogConfig' {samplingRate} -> samplingRate) (\s@RealtimeLogConfig' {} a -> s {samplingRate = a} :: RealtimeLogConfig)

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data for this real-time log configuration.
realtimeLogConfig_endPoints :: Lens.Lens' RealtimeLogConfig [EndPoint]
realtimeLogConfig_endPoints = Lens.lens (\RealtimeLogConfig' {endPoints} -> endPoints) (\s@RealtimeLogConfig' {} a -> s {endPoints = a} :: RealtimeLogConfig) Prelude.. Lens._Coerce

-- | A list of fields that are included in each real-time log record. In an
-- API response, the fields are provided in the same order in which they
-- are sent to the Amazon Kinesis data stream.
--
-- For more information about fields, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields>
-- in the /Amazon CloudFront Developer Guide/.
realtimeLogConfig_fields :: Lens.Lens' RealtimeLogConfig [Prelude.Text]
realtimeLogConfig_fields = Lens.lens (\RealtimeLogConfig' {fields} -> fields) (\s@RealtimeLogConfig' {} a -> s {fields = a} :: RealtimeLogConfig) Prelude.. Lens._Coerce

instance Core.FromXML RealtimeLogConfig where
  parseXML x =
    RealtimeLogConfig'
      Prelude.<$> (x Core..@ "ARN")
      Prelude.<*> (x Core..@ "Name")
      Prelude.<*> (x Core..@ "SamplingRate")
      Prelude.<*> ( x Core..@? "EndPoints" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )
      Prelude.<*> ( x Core..@? "Fields" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "Field"
                  )

instance Prelude.Hashable RealtimeLogConfig

instance Prelude.NFData RealtimeLogConfig

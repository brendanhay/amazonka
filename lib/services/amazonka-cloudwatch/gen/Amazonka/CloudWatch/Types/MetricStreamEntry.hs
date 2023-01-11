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
-- Module      : Amazonka.CloudWatch.Types.MetricStreamEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamEntry where

import Amazonka.CloudWatch.Types.MetricStreamOutputFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the configuration information about one metric
-- stream.
--
-- /See:/ 'newMetricStreamEntry' smart constructor.
data MetricStreamEntry = MetricStreamEntry'
  { -- | The ARN of the metric stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date that the metric stream was originally created.
    creationDate :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the Kinesis Firehose devlivery stream that is used for this
    -- metric stream.
    firehoseArn :: Prelude.Maybe Prelude.Text,
    -- | The date that the configuration of this metric stream was most recently
    -- updated.
    lastUpdateDate :: Prelude.Maybe Data.ISO8601,
    -- | The name of the metric stream.
    name :: Prelude.Maybe Prelude.Text,
    -- | The output format of this metric stream. Valid values are @json@ and
    -- @opentelemetry0.7@.
    outputFormat :: Prelude.Maybe MetricStreamOutputFormat,
    -- | The current state of this stream. Valid values are @running@ and
    -- @stopped@.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStreamEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'metricStreamEntry_arn' - The ARN of the metric stream.
--
-- 'creationDate', 'metricStreamEntry_creationDate' - The date that the metric stream was originally created.
--
-- 'firehoseArn', 'metricStreamEntry_firehoseArn' - The ARN of the Kinesis Firehose devlivery stream that is used for this
-- metric stream.
--
-- 'lastUpdateDate', 'metricStreamEntry_lastUpdateDate' - The date that the configuration of this metric stream was most recently
-- updated.
--
-- 'name', 'metricStreamEntry_name' - The name of the metric stream.
--
-- 'outputFormat', 'metricStreamEntry_outputFormat' - The output format of this metric stream. Valid values are @json@ and
-- @opentelemetry0.7@.
--
-- 'state', 'metricStreamEntry_state' - The current state of this stream. Valid values are @running@ and
-- @stopped@.
newMetricStreamEntry ::
  MetricStreamEntry
newMetricStreamEntry =
  MetricStreamEntry'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      firehoseArn = Prelude.Nothing,
      lastUpdateDate = Prelude.Nothing,
      name = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The ARN of the metric stream.
metricStreamEntry_arn :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.Text)
metricStreamEntry_arn = Lens.lens (\MetricStreamEntry' {arn} -> arn) (\s@MetricStreamEntry' {} a -> s {arn = a} :: MetricStreamEntry)

-- | The date that the metric stream was originally created.
metricStreamEntry_creationDate :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.UTCTime)
metricStreamEntry_creationDate = Lens.lens (\MetricStreamEntry' {creationDate} -> creationDate) (\s@MetricStreamEntry' {} a -> s {creationDate = a} :: MetricStreamEntry) Prelude.. Lens.mapping Data._Time

-- | The ARN of the Kinesis Firehose devlivery stream that is used for this
-- metric stream.
metricStreamEntry_firehoseArn :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.Text)
metricStreamEntry_firehoseArn = Lens.lens (\MetricStreamEntry' {firehoseArn} -> firehoseArn) (\s@MetricStreamEntry' {} a -> s {firehoseArn = a} :: MetricStreamEntry)

-- | The date that the configuration of this metric stream was most recently
-- updated.
metricStreamEntry_lastUpdateDate :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.UTCTime)
metricStreamEntry_lastUpdateDate = Lens.lens (\MetricStreamEntry' {lastUpdateDate} -> lastUpdateDate) (\s@MetricStreamEntry' {} a -> s {lastUpdateDate = a} :: MetricStreamEntry) Prelude.. Lens.mapping Data._Time

-- | The name of the metric stream.
metricStreamEntry_name :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.Text)
metricStreamEntry_name = Lens.lens (\MetricStreamEntry' {name} -> name) (\s@MetricStreamEntry' {} a -> s {name = a} :: MetricStreamEntry)

-- | The output format of this metric stream. Valid values are @json@ and
-- @opentelemetry0.7@.
metricStreamEntry_outputFormat :: Lens.Lens' MetricStreamEntry (Prelude.Maybe MetricStreamOutputFormat)
metricStreamEntry_outputFormat = Lens.lens (\MetricStreamEntry' {outputFormat} -> outputFormat) (\s@MetricStreamEntry' {} a -> s {outputFormat = a} :: MetricStreamEntry)

-- | The current state of this stream. Valid values are @running@ and
-- @stopped@.
metricStreamEntry_state :: Lens.Lens' MetricStreamEntry (Prelude.Maybe Prelude.Text)
metricStreamEntry_state = Lens.lens (\MetricStreamEntry' {state} -> state) (\s@MetricStreamEntry' {} a -> s {state = a} :: MetricStreamEntry)

instance Data.FromXML MetricStreamEntry where
  parseXML x =
    MetricStreamEntry'
      Prelude.<$> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "CreationDate")
      Prelude.<*> (x Data..@? "FirehoseArn")
      Prelude.<*> (x Data..@? "LastUpdateDate")
      Prelude.<*> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "OutputFormat")
      Prelude.<*> (x Data..@? "State")

instance Prelude.Hashable MetricStreamEntry where
  hashWithSalt _salt MetricStreamEntry' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` firehoseArn
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` state

instance Prelude.NFData MetricStreamEntry where
  rnf MetricStreamEntry' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf firehoseArn
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf state

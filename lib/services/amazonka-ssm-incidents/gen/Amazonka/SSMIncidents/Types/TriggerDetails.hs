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
-- Module      : Amazonka.SSMIncidents.Types.TriggerDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.TriggerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about what caused the incident to be created in Incident
-- Manager.
--
-- /See:/ 'newTriggerDetails' smart constructor.
data TriggerDetails = TriggerDetails'
  { -- | The Amazon Resource Name (ARN) of the source that detected the incident.
    triggerArn :: Prelude.Maybe Prelude.Text,
    -- | Raw data passed from either Amazon EventBridge, Amazon CloudWatch, or
    -- Incident Manager when an incident is created.
    rawData :: Prelude.Maybe Prelude.Text,
    -- | Identifies the service that sourced the event. All events sourced from
    -- within Amazon Web Services begin with \"@aws.@\" Customer-generated
    -- events can have any value here, as long as it doesn\'t begin with
    -- \"@aws.@\" We recommend the use of Java package-name style reverse
    -- domain-name strings.
    source :: Prelude.Text,
    -- | The time that the incident was detected.
    timestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerArn', 'triggerDetails_triggerArn' - The Amazon Resource Name (ARN) of the source that detected the incident.
--
-- 'rawData', 'triggerDetails_rawData' - Raw data passed from either Amazon EventBridge, Amazon CloudWatch, or
-- Incident Manager when an incident is created.
--
-- 'source', 'triggerDetails_source' - Identifies the service that sourced the event. All events sourced from
-- within Amazon Web Services begin with \"@aws.@\" Customer-generated
-- events can have any value here, as long as it doesn\'t begin with
-- \"@aws.@\" We recommend the use of Java package-name style reverse
-- domain-name strings.
--
-- 'timestamp', 'triggerDetails_timestamp' - The time that the incident was detected.
newTriggerDetails ::
  -- | 'source'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  TriggerDetails
newTriggerDetails pSource_ pTimestamp_ =
  TriggerDetails'
    { triggerArn = Prelude.Nothing,
      rawData = Prelude.Nothing,
      source = pSource_,
      timestamp = Core._Time Lens.# pTimestamp_
    }

-- | The Amazon Resource Name (ARN) of the source that detected the incident.
triggerDetails_triggerArn :: Lens.Lens' TriggerDetails (Prelude.Maybe Prelude.Text)
triggerDetails_triggerArn = Lens.lens (\TriggerDetails' {triggerArn} -> triggerArn) (\s@TriggerDetails' {} a -> s {triggerArn = a} :: TriggerDetails)

-- | Raw data passed from either Amazon EventBridge, Amazon CloudWatch, or
-- Incident Manager when an incident is created.
triggerDetails_rawData :: Lens.Lens' TriggerDetails (Prelude.Maybe Prelude.Text)
triggerDetails_rawData = Lens.lens (\TriggerDetails' {rawData} -> rawData) (\s@TriggerDetails' {} a -> s {rawData = a} :: TriggerDetails)

-- | Identifies the service that sourced the event. All events sourced from
-- within Amazon Web Services begin with \"@aws.@\" Customer-generated
-- events can have any value here, as long as it doesn\'t begin with
-- \"@aws.@\" We recommend the use of Java package-name style reverse
-- domain-name strings.
triggerDetails_source :: Lens.Lens' TriggerDetails Prelude.Text
triggerDetails_source = Lens.lens (\TriggerDetails' {source} -> source) (\s@TriggerDetails' {} a -> s {source = a} :: TriggerDetails)

-- | The time that the incident was detected.
triggerDetails_timestamp :: Lens.Lens' TriggerDetails Prelude.UTCTime
triggerDetails_timestamp = Lens.lens (\TriggerDetails' {timestamp} -> timestamp) (\s@TriggerDetails' {} a -> s {timestamp = a} :: TriggerDetails) Prelude.. Core._Time

instance Prelude.Hashable TriggerDetails where
  hashWithSalt _salt TriggerDetails' {..} =
    _salt `Prelude.hashWithSalt` triggerArn
      `Prelude.hashWithSalt` rawData
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TriggerDetails where
  rnf TriggerDetails' {..} =
    Prelude.rnf triggerArn
      `Prelude.seq` Prelude.rnf rawData
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf timestamp

instance Core.ToJSON TriggerDetails where
  toJSON TriggerDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("triggerArn" Core..=) Prelude.<$> triggerArn,
            ("rawData" Core..=) Prelude.<$> rawData,
            Prelude.Just ("source" Core..= source),
            Prelude.Just ("timestamp" Core..= timestamp)
          ]
      )

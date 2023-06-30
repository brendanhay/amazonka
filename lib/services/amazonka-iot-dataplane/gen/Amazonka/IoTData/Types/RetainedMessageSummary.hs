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
-- Module      : Amazonka.IoTData.Types.RetainedMessageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTData.Types.RetainedMessageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a single retained message.
--
-- /See:/ 'newRetainedMessageSummary' smart constructor.
data RetainedMessageSummary = RetainedMessageSummary'
  { -- | The Epoch date and time, in milliseconds, when the retained message was
    -- stored by IoT.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The size of the retained message\'s payload in bytes.
    payloadSize :: Prelude.Maybe Prelude.Integer,
    -- | The quality of service (QoS) level used to publish the retained message.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | The topic name to which the retained message was published.
    topic :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetainedMessageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'retainedMessageSummary_lastModifiedTime' - The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
--
-- 'payloadSize', 'retainedMessageSummary_payloadSize' - The size of the retained message\'s payload in bytes.
--
-- 'qos', 'retainedMessageSummary_qos' - The quality of service (QoS) level used to publish the retained message.
--
-- 'topic', 'retainedMessageSummary_topic' - The topic name to which the retained message was published.
newRetainedMessageSummary ::
  RetainedMessageSummary
newRetainedMessageSummary =
  RetainedMessageSummary'
    { lastModifiedTime =
        Prelude.Nothing,
      payloadSize = Prelude.Nothing,
      qos = Prelude.Nothing,
      topic = Prelude.Nothing
    }

-- | The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
retainedMessageSummary_lastModifiedTime :: Lens.Lens' RetainedMessageSummary (Prelude.Maybe Prelude.Integer)
retainedMessageSummary_lastModifiedTime = Lens.lens (\RetainedMessageSummary' {lastModifiedTime} -> lastModifiedTime) (\s@RetainedMessageSummary' {} a -> s {lastModifiedTime = a} :: RetainedMessageSummary)

-- | The size of the retained message\'s payload in bytes.
retainedMessageSummary_payloadSize :: Lens.Lens' RetainedMessageSummary (Prelude.Maybe Prelude.Integer)
retainedMessageSummary_payloadSize = Lens.lens (\RetainedMessageSummary' {payloadSize} -> payloadSize) (\s@RetainedMessageSummary' {} a -> s {payloadSize = a} :: RetainedMessageSummary)

-- | The quality of service (QoS) level used to publish the retained message.
retainedMessageSummary_qos :: Lens.Lens' RetainedMessageSummary (Prelude.Maybe Prelude.Natural)
retainedMessageSummary_qos = Lens.lens (\RetainedMessageSummary' {qos} -> qos) (\s@RetainedMessageSummary' {} a -> s {qos = a} :: RetainedMessageSummary)

-- | The topic name to which the retained message was published.
retainedMessageSummary_topic :: Lens.Lens' RetainedMessageSummary (Prelude.Maybe Prelude.Text)
retainedMessageSummary_topic = Lens.lens (\RetainedMessageSummary' {topic} -> topic) (\s@RetainedMessageSummary' {} a -> s {topic = a} :: RetainedMessageSummary)

instance Data.FromJSON RetainedMessageSummary where
  parseJSON =
    Data.withObject
      "RetainedMessageSummary"
      ( \x ->
          RetainedMessageSummary'
            Prelude.<$> (x Data..:? "lastModifiedTime")
            Prelude.<*> (x Data..:? "payloadSize")
            Prelude.<*> (x Data..:? "qos")
            Prelude.<*> (x Data..:? "topic")
      )

instance Prelude.Hashable RetainedMessageSummary where
  hashWithSalt _salt RetainedMessageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` payloadSize
      `Prelude.hashWithSalt` qos
      `Prelude.hashWithSalt` topic

instance Prelude.NFData RetainedMessageSummary where
  rnf RetainedMessageSummary' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf payloadSize
      `Prelude.seq` Prelude.rnf qos
      `Prelude.seq` Prelude.rnf topic

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
-- Module      : Amazonka.Inspector.Types.TelemetryMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.TelemetryMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata about the Amazon Inspector application data metrics
-- collected by the agent. This data type is used as the response element
-- in the GetTelemetryMetadata action.
--
-- /See:/ 'newTelemetryMetadata' smart constructor.
data TelemetryMetadata = TelemetryMetadata'
  { -- | The data size of messages that the agent sends to the Amazon Inspector
    -- service.
    dataSize :: Prelude.Maybe Prelude.Integer,
    -- | A specific type of behavioral data that is collected by the agent.
    messageType :: Prelude.Text,
    -- | The count of messages that the agent sends to the Amazon Inspector
    -- service.
    count :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TelemetryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSize', 'telemetryMetadata_dataSize' - The data size of messages that the agent sends to the Amazon Inspector
-- service.
--
-- 'messageType', 'telemetryMetadata_messageType' - A specific type of behavioral data that is collected by the agent.
--
-- 'count', 'telemetryMetadata_count' - The count of messages that the agent sends to the Amazon Inspector
-- service.
newTelemetryMetadata ::
  -- | 'messageType'
  Prelude.Text ->
  -- | 'count'
  Prelude.Integer ->
  TelemetryMetadata
newTelemetryMetadata pMessageType_ pCount_ =
  TelemetryMetadata'
    { dataSize = Prelude.Nothing,
      messageType = pMessageType_,
      count = pCount_
    }

-- | The data size of messages that the agent sends to the Amazon Inspector
-- service.
telemetryMetadata_dataSize :: Lens.Lens' TelemetryMetadata (Prelude.Maybe Prelude.Integer)
telemetryMetadata_dataSize = Lens.lens (\TelemetryMetadata' {dataSize} -> dataSize) (\s@TelemetryMetadata' {} a -> s {dataSize = a} :: TelemetryMetadata)

-- | A specific type of behavioral data that is collected by the agent.
telemetryMetadata_messageType :: Lens.Lens' TelemetryMetadata Prelude.Text
telemetryMetadata_messageType = Lens.lens (\TelemetryMetadata' {messageType} -> messageType) (\s@TelemetryMetadata' {} a -> s {messageType = a} :: TelemetryMetadata)

-- | The count of messages that the agent sends to the Amazon Inspector
-- service.
telemetryMetadata_count :: Lens.Lens' TelemetryMetadata Prelude.Integer
telemetryMetadata_count = Lens.lens (\TelemetryMetadata' {count} -> count) (\s@TelemetryMetadata' {} a -> s {count = a} :: TelemetryMetadata)

instance Core.FromJSON TelemetryMetadata where
  parseJSON =
    Core.withObject
      "TelemetryMetadata"
      ( \x ->
          TelemetryMetadata'
            Prelude.<$> (x Core..:? "dataSize")
            Prelude.<*> (x Core..: "messageType")
            Prelude.<*> (x Core..: "count")
      )

instance Prelude.Hashable TelemetryMetadata where
  hashWithSalt _salt TelemetryMetadata' {..} =
    _salt `Prelude.hashWithSalt` dataSize
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` count

instance Prelude.NFData TelemetryMetadata where
  rnf TelemetryMetadata' {..} =
    Prelude.rnf dataSize
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf count

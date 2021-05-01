{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Inspector.Types.TelemetryMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TelemetryMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TelemetryMetadata where
  parseJSON =
    Prelude.withObject
      "TelemetryMetadata"
      ( \x ->
          TelemetryMetadata'
            Prelude.<$> (x Prelude..:? "dataSize")
            Prelude.<*> (x Prelude..: "messageType")
            Prelude.<*> (x Prelude..: "count")
      )

instance Prelude.Hashable TelemetryMetadata

instance Prelude.NFData TelemetryMetadata

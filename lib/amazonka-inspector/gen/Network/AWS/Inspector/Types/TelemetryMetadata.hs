{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.TelemetryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TelemetryMetadata
  ( TelemetryMetadata (..),

    -- * Smart constructor
    mkTelemetryMetadata,

    -- * Lenses
    tmMessageType,
    tmCount,
    tmDataSize,
  )
where

import qualified Network.AWS.Inspector.Types.MessageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata about the Amazon Inspector application data metrics collected by the agent. This data type is used as the response element in the 'GetTelemetryMetadata' action.
--
-- /See:/ 'mkTelemetryMetadata' smart constructor.
data TelemetryMetadata = TelemetryMetadata'
  { -- | A specific type of behavioral data that is collected by the agent.
    messageType :: Types.MessageType,
    -- | The count of messages that the agent sends to the Amazon Inspector service.
    count :: Core.Integer,
    -- | The data size of messages that the agent sends to the Amazon Inspector service.
    dataSize :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TelemetryMetadata' value with any optional fields omitted.
mkTelemetryMetadata ::
  -- | 'messageType'
  Types.MessageType ->
  -- | 'count'
  Core.Integer ->
  TelemetryMetadata
mkTelemetryMetadata messageType count =
  TelemetryMetadata' {messageType, count, dataSize = Core.Nothing}

-- | A specific type of behavioral data that is collected by the agent.
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmMessageType :: Lens.Lens' TelemetryMetadata Types.MessageType
tmMessageType = Lens.field @"messageType"
{-# DEPRECATED tmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The count of messages that the agent sends to the Amazon Inspector service.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCount :: Lens.Lens' TelemetryMetadata Core.Integer
tmCount = Lens.field @"count"
{-# DEPRECATED tmCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The data size of messages that the agent sends to the Amazon Inspector service.
--
-- /Note:/ Consider using 'dataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmDataSize :: Lens.Lens' TelemetryMetadata (Core.Maybe Core.Integer)
tmDataSize = Lens.field @"dataSize"
{-# DEPRECATED tmDataSize "Use generic-lens or generic-optics with 'dataSize' instead." #-}

instance Core.FromJSON TelemetryMetadata where
  parseJSON =
    Core.withObject "TelemetryMetadata" Core.$
      \x ->
        TelemetryMetadata'
          Core.<$> (x Core..: "messageType")
          Core.<*> (x Core..: "count")
          Core.<*> (x Core..:? "dataSize")

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
    tmDataSize,
    tmMessageType,
    tmCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata about the Amazon Inspector application data metrics collected by the agent. This data type is used as the response element in the 'GetTelemetryMetadata' action.
--
-- /See:/ 'mkTelemetryMetadata' smart constructor.
data TelemetryMetadata = TelemetryMetadata'
  { dataSize ::
      Lude.Maybe Lude.Integer,
    messageType :: Lude.Text,
    count :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TelemetryMetadata' with the minimum fields required to make a request.
--
-- * 'count' - The count of messages that the agent sends to the Amazon Inspector service.
-- * 'dataSize' - The data size of messages that the agent sends to the Amazon Inspector service.
-- * 'messageType' - A specific type of behavioral data that is collected by the agent.
mkTelemetryMetadata ::
  -- | 'messageType'
  Lude.Text ->
  -- | 'count'
  Lude.Integer ->
  TelemetryMetadata
mkTelemetryMetadata pMessageType_ pCount_ =
  TelemetryMetadata'
    { dataSize = Lude.Nothing,
      messageType = pMessageType_,
      count = pCount_
    }

-- | The data size of messages that the agent sends to the Amazon Inspector service.
--
-- /Note:/ Consider using 'dataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmDataSize :: Lens.Lens' TelemetryMetadata (Lude.Maybe Lude.Integer)
tmDataSize = Lens.lens (dataSize :: TelemetryMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {dataSize = a} :: TelemetryMetadata)
{-# DEPRECATED tmDataSize "Use generic-lens or generic-optics with 'dataSize' instead." #-}

-- | A specific type of behavioral data that is collected by the agent.
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmMessageType :: Lens.Lens' TelemetryMetadata Lude.Text
tmMessageType = Lens.lens (messageType :: TelemetryMetadata -> Lude.Text) (\s a -> s {messageType = a} :: TelemetryMetadata)
{-# DEPRECATED tmMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The count of messages that the agent sends to the Amazon Inspector service.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCount :: Lens.Lens' TelemetryMetadata Lude.Integer
tmCount = Lens.lens (count :: TelemetryMetadata -> Lude.Integer) (\s a -> s {count = a} :: TelemetryMetadata)
{-# DEPRECATED tmCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON TelemetryMetadata where
  parseJSON =
    Lude.withObject
      "TelemetryMetadata"
      ( \x ->
          TelemetryMetadata'
            Lude.<$> (x Lude..:? "dataSize")
            Lude.<*> (x Lude..: "messageType")
            Lude.<*> (x Lude..: "count")
      )

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
-- Module      : Network.AWS.Connect.Types.OutboundCallerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.OutboundCallerConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The outbound caller ID name, number, and outbound whisper flow.
--
-- /See:/ 'newOutboundCallerConfig' smart constructor.
data OutboundCallerConfig = OutboundCallerConfig'
  { -- | The caller ID number.
    outboundCallerIdNumberId :: Core.Maybe Core.Text,
    -- | The outbound whisper flow to be used during an outbound call.
    outboundFlowId :: Core.Maybe Core.Text,
    -- | The caller ID name.
    outboundCallerIdName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutboundCallerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outboundCallerIdNumberId', 'outboundCallerConfig_outboundCallerIdNumberId' - The caller ID number.
--
-- 'outboundFlowId', 'outboundCallerConfig_outboundFlowId' - The outbound whisper flow to be used during an outbound call.
--
-- 'outboundCallerIdName', 'outboundCallerConfig_outboundCallerIdName' - The caller ID name.
newOutboundCallerConfig ::
  OutboundCallerConfig
newOutboundCallerConfig =
  OutboundCallerConfig'
    { outboundCallerIdNumberId =
        Core.Nothing,
      outboundFlowId = Core.Nothing,
      outboundCallerIdName = Core.Nothing
    }

-- | The caller ID number.
outboundCallerConfig_outboundCallerIdNumberId :: Lens.Lens' OutboundCallerConfig (Core.Maybe Core.Text)
outboundCallerConfig_outboundCallerIdNumberId = Lens.lens (\OutboundCallerConfig' {outboundCallerIdNumberId} -> outboundCallerIdNumberId) (\s@OutboundCallerConfig' {} a -> s {outboundCallerIdNumberId = a} :: OutboundCallerConfig)

-- | The outbound whisper flow to be used during an outbound call.
outboundCallerConfig_outboundFlowId :: Lens.Lens' OutboundCallerConfig (Core.Maybe Core.Text)
outboundCallerConfig_outboundFlowId = Lens.lens (\OutboundCallerConfig' {outboundFlowId} -> outboundFlowId) (\s@OutboundCallerConfig' {} a -> s {outboundFlowId = a} :: OutboundCallerConfig)

-- | The caller ID name.
outboundCallerConfig_outboundCallerIdName :: Lens.Lens' OutboundCallerConfig (Core.Maybe Core.Text)
outboundCallerConfig_outboundCallerIdName = Lens.lens (\OutboundCallerConfig' {outboundCallerIdName} -> outboundCallerIdName) (\s@OutboundCallerConfig' {} a -> s {outboundCallerIdName = a} :: OutboundCallerConfig)

instance Core.FromJSON OutboundCallerConfig where
  parseJSON =
    Core.withObject
      "OutboundCallerConfig"
      ( \x ->
          OutboundCallerConfig'
            Core.<$> (x Core..:? "OutboundCallerIdNumberId")
            Core.<*> (x Core..:? "OutboundFlowId")
            Core.<*> (x Core..:? "OutboundCallerIdName")
      )

instance Core.Hashable OutboundCallerConfig

instance Core.NFData OutboundCallerConfig

instance Core.ToJSON OutboundCallerConfig where
  toJSON OutboundCallerConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutboundCallerIdNumberId" Core..=)
              Core.<$> outboundCallerIdNumberId,
            ("OutboundFlowId" Core..=) Core.<$> outboundFlowId,
            ("OutboundCallerIdName" Core..=)
              Core.<$> outboundCallerIdName
          ]
      )

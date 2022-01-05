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
-- Module      : Amazonka.Connect.Types.OutboundCallerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.OutboundCallerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The outbound caller ID name, number, and outbound whisper flow.
--
-- /See:/ 'newOutboundCallerConfig' smart constructor.
data OutboundCallerConfig = OutboundCallerConfig'
  { -- | The caller ID number.
    outboundCallerIdNumberId :: Prelude.Maybe Prelude.Text,
    -- | The caller ID name.
    outboundCallerIdName :: Prelude.Maybe Prelude.Text,
    -- | The outbound whisper flow to be used during an outbound call.
    outboundFlowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'outboundCallerIdName', 'outboundCallerConfig_outboundCallerIdName' - The caller ID name.
--
-- 'outboundFlowId', 'outboundCallerConfig_outboundFlowId' - The outbound whisper flow to be used during an outbound call.
newOutboundCallerConfig ::
  OutboundCallerConfig
newOutboundCallerConfig =
  OutboundCallerConfig'
    { outboundCallerIdNumberId =
        Prelude.Nothing,
      outboundCallerIdName = Prelude.Nothing,
      outboundFlowId = Prelude.Nothing
    }

-- | The caller ID number.
outboundCallerConfig_outboundCallerIdNumberId :: Lens.Lens' OutboundCallerConfig (Prelude.Maybe Prelude.Text)
outboundCallerConfig_outboundCallerIdNumberId = Lens.lens (\OutboundCallerConfig' {outboundCallerIdNumberId} -> outboundCallerIdNumberId) (\s@OutboundCallerConfig' {} a -> s {outboundCallerIdNumberId = a} :: OutboundCallerConfig)

-- | The caller ID name.
outboundCallerConfig_outboundCallerIdName :: Lens.Lens' OutboundCallerConfig (Prelude.Maybe Prelude.Text)
outboundCallerConfig_outboundCallerIdName = Lens.lens (\OutboundCallerConfig' {outboundCallerIdName} -> outboundCallerIdName) (\s@OutboundCallerConfig' {} a -> s {outboundCallerIdName = a} :: OutboundCallerConfig)

-- | The outbound whisper flow to be used during an outbound call.
outboundCallerConfig_outboundFlowId :: Lens.Lens' OutboundCallerConfig (Prelude.Maybe Prelude.Text)
outboundCallerConfig_outboundFlowId = Lens.lens (\OutboundCallerConfig' {outboundFlowId} -> outboundFlowId) (\s@OutboundCallerConfig' {} a -> s {outboundFlowId = a} :: OutboundCallerConfig)

instance Core.FromJSON OutboundCallerConfig where
  parseJSON =
    Core.withObject
      "OutboundCallerConfig"
      ( \x ->
          OutboundCallerConfig'
            Prelude.<$> (x Core..:? "OutboundCallerIdNumberId")
            Prelude.<*> (x Core..:? "OutboundCallerIdName")
            Prelude.<*> (x Core..:? "OutboundFlowId")
      )

instance Prelude.Hashable OutboundCallerConfig where
  hashWithSalt _salt OutboundCallerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` outboundCallerIdNumberId
      `Prelude.hashWithSalt` outboundCallerIdName
      `Prelude.hashWithSalt` outboundFlowId

instance Prelude.NFData OutboundCallerConfig where
  rnf OutboundCallerConfig' {..} =
    Prelude.rnf outboundCallerIdNumberId
      `Prelude.seq` Prelude.rnf outboundCallerIdName
      `Prelude.seq` Prelude.rnf outboundFlowId

instance Core.ToJSON OutboundCallerConfig where
  toJSON OutboundCallerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutboundCallerIdNumberId" Core..=)
              Prelude.<$> outboundCallerIdNumberId,
            ("OutboundCallerIdName" Core..=)
              Prelude.<$> outboundCallerIdName,
            ("OutboundFlowId" Core..=)
              Prelude.<$> outboundFlowId
          ]
      )

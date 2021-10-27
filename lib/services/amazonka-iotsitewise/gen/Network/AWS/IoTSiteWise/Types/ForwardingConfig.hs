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
-- Module      : Network.AWS.IoTSiteWise.Types.ForwardingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.ForwardingConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.ForwardingConfigState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The forwarding configuration for a given property.
--
-- /See:/ 'newForwardingConfig' smart constructor.
data ForwardingConfig = ForwardingConfig'
  { -- | The forwarding state for the given property.
    state :: ForwardingConfigState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForwardingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'forwardingConfig_state' - The forwarding state for the given property.
newForwardingConfig ::
  -- | 'state'
  ForwardingConfigState ->
  ForwardingConfig
newForwardingConfig pState_ =
  ForwardingConfig' {state = pState_}

-- | The forwarding state for the given property.
forwardingConfig_state :: Lens.Lens' ForwardingConfig ForwardingConfigState
forwardingConfig_state = Lens.lens (\ForwardingConfig' {state} -> state) (\s@ForwardingConfig' {} a -> s {state = a} :: ForwardingConfig)

instance Core.FromJSON ForwardingConfig where
  parseJSON =
    Core.withObject
      "ForwardingConfig"
      ( \x ->
          ForwardingConfig' Prelude.<$> (x Core..: "state")
      )

instance Prelude.Hashable ForwardingConfig

instance Prelude.NFData ForwardingConfig

instance Core.ToJSON ForwardingConfig where
  toJSON ForwardingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("state" Core..= state)]
      )

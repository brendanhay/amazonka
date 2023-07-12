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
-- Module      : Amazonka.IoTSiteWise.Types.ForwardingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ForwardingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ForwardingConfigState
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON ForwardingConfig where
  parseJSON =
    Data.withObject
      "ForwardingConfig"
      ( \x ->
          ForwardingConfig' Prelude.<$> (x Data..: "state")
      )

instance Prelude.Hashable ForwardingConfig where
  hashWithSalt _salt ForwardingConfig' {..} =
    _salt `Prelude.hashWithSalt` state

instance Prelude.NFData ForwardingConfig where
  rnf ForwardingConfig' {..} = Prelude.rnf state

instance Data.ToJSON ForwardingConfig where
  toJSON ForwardingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("state" Data..= state)]
      )

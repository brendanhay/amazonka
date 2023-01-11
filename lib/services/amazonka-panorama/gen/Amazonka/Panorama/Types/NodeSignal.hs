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
-- Module      : Amazonka.Panorama.Types.NodeSignal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeSignal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.NodeSignalValue
import qualified Amazonka.Prelude as Prelude

-- | A signal to a camera node to start or stop processing video.
--
-- /See:/ 'newNodeSignal' smart constructor.
data NodeSignal = NodeSignal'
  { -- | The camera node\'s name, from the application manifest.
    nodeInstanceId :: Prelude.Text,
    -- | The signal value.
    signal :: NodeSignalValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeSignal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeInstanceId', 'nodeSignal_nodeInstanceId' - The camera node\'s name, from the application manifest.
--
-- 'signal', 'nodeSignal_signal' - The signal value.
newNodeSignal ::
  -- | 'nodeInstanceId'
  Prelude.Text ->
  -- | 'signal'
  NodeSignalValue ->
  NodeSignal
newNodeSignal pNodeInstanceId_ pSignal_ =
  NodeSignal'
    { nodeInstanceId = pNodeInstanceId_,
      signal = pSignal_
    }

-- | The camera node\'s name, from the application manifest.
nodeSignal_nodeInstanceId :: Lens.Lens' NodeSignal Prelude.Text
nodeSignal_nodeInstanceId = Lens.lens (\NodeSignal' {nodeInstanceId} -> nodeInstanceId) (\s@NodeSignal' {} a -> s {nodeInstanceId = a} :: NodeSignal)

-- | The signal value.
nodeSignal_signal :: Lens.Lens' NodeSignal NodeSignalValue
nodeSignal_signal = Lens.lens (\NodeSignal' {signal} -> signal) (\s@NodeSignal' {} a -> s {signal = a} :: NodeSignal)

instance Prelude.Hashable NodeSignal where
  hashWithSalt _salt NodeSignal' {..} =
    _salt `Prelude.hashWithSalt` nodeInstanceId
      `Prelude.hashWithSalt` signal

instance Prelude.NFData NodeSignal where
  rnf NodeSignal' {..} =
    Prelude.rnf nodeInstanceId
      `Prelude.seq` Prelude.rnf signal

instance Data.ToJSON NodeSignal where
  toJSON NodeSignal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NodeInstanceId" Data..= nodeInstanceId),
            Prelude.Just ("Signal" Data..= signal)
          ]
      )

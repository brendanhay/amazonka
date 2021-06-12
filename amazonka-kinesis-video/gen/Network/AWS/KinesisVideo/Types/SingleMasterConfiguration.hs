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
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.SingleMasterConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that contains the configuration for the @SINGLE_MASTER@
-- channel type.
--
-- /See:/ 'newSingleMasterConfiguration' smart constructor.
data SingleMasterConfiguration = SingleMasterConfiguration'
  { -- | The period of time a signaling channel retains underlivered messages
    -- before they are discarded.
    messageTtlSeconds :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SingleMasterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageTtlSeconds', 'singleMasterConfiguration_messageTtlSeconds' - The period of time a signaling channel retains underlivered messages
-- before they are discarded.
newSingleMasterConfiguration ::
  SingleMasterConfiguration
newSingleMasterConfiguration =
  SingleMasterConfiguration'
    { messageTtlSeconds =
        Core.Nothing
    }

-- | The period of time a signaling channel retains underlivered messages
-- before they are discarded.
singleMasterConfiguration_messageTtlSeconds :: Lens.Lens' SingleMasterConfiguration (Core.Maybe Core.Natural)
singleMasterConfiguration_messageTtlSeconds = Lens.lens (\SingleMasterConfiguration' {messageTtlSeconds} -> messageTtlSeconds) (\s@SingleMasterConfiguration' {} a -> s {messageTtlSeconds = a} :: SingleMasterConfiguration)

instance Core.FromJSON SingleMasterConfiguration where
  parseJSON =
    Core.withObject
      "SingleMasterConfiguration"
      ( \x ->
          SingleMasterConfiguration'
            Core.<$> (x Core..:? "MessageTtlSeconds")
      )

instance Core.Hashable SingleMasterConfiguration

instance Core.NFData SingleMasterConfiguration

instance Core.ToJSON SingleMasterConfiguration where
  toJSON SingleMasterConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MessageTtlSeconds" Core..=)
              Core.<$> messageTtlSeconds
          ]
      )

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
-- Module      : Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupStickinessConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the target group stickiness for a rule.
--
-- /See:/ 'newTargetGroupStickinessConfig' smart constructor.
data TargetGroupStickinessConfig = TargetGroupStickinessConfig'
  { -- | Indicates whether target group stickiness is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The time period, in seconds, during which requests from a client should
    -- be routed to the same target group. The range is 1-604800 seconds (7
    -- days).
    durationSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupStickinessConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'targetGroupStickinessConfig_enabled' - Indicates whether target group stickiness is enabled.
--
-- 'durationSeconds', 'targetGroupStickinessConfig_durationSeconds' - The time period, in seconds, during which requests from a client should
-- be routed to the same target group. The range is 1-604800 seconds (7
-- days).
newTargetGroupStickinessConfig ::
  TargetGroupStickinessConfig
newTargetGroupStickinessConfig =
  TargetGroupStickinessConfig'
    { enabled =
        Prelude.Nothing,
      durationSeconds = Prelude.Nothing
    }

-- | Indicates whether target group stickiness is enabled.
targetGroupStickinessConfig_enabled :: Lens.Lens' TargetGroupStickinessConfig (Prelude.Maybe Prelude.Bool)
targetGroupStickinessConfig_enabled = Lens.lens (\TargetGroupStickinessConfig' {enabled} -> enabled) (\s@TargetGroupStickinessConfig' {} a -> s {enabled = a} :: TargetGroupStickinessConfig)

-- | The time period, in seconds, during which requests from a client should
-- be routed to the same target group. The range is 1-604800 seconds (7
-- days).
targetGroupStickinessConfig_durationSeconds :: Lens.Lens' TargetGroupStickinessConfig (Prelude.Maybe Prelude.Int)
targetGroupStickinessConfig_durationSeconds = Lens.lens (\TargetGroupStickinessConfig' {durationSeconds} -> durationSeconds) (\s@TargetGroupStickinessConfig' {} a -> s {durationSeconds = a} :: TargetGroupStickinessConfig)

instance Prelude.FromXML TargetGroupStickinessConfig where
  parseXML x =
    TargetGroupStickinessConfig'
      Prelude.<$> (x Prelude..@? "Enabled")
      Prelude.<*> (x Prelude..@? "DurationSeconds")

instance Prelude.Hashable TargetGroupStickinessConfig

instance Prelude.NFData TargetGroupStickinessConfig

instance Prelude.ToQuery TargetGroupStickinessConfig where
  toQuery TargetGroupStickinessConfig' {..} =
    Prelude.mconcat
      [ "Enabled" Prelude.=: enabled,
        "DurationSeconds" Prelude.=: durationSeconds
      ]

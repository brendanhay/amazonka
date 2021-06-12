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
-- Module      : Network.AWS.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightsConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The structure containing configurations related to insights.
--
-- /See:/ 'newInsightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { -- | Set the NotificationsEnabled value to true to enable insights
    -- notifications. Notifications can only be enabled on a group with
    -- InsightsEnabled set to true.
    notificationsEnabled :: Core.Maybe Core.Bool,
    -- | Set the InsightsEnabled value to true to enable insights or false to
    -- disable insights.
    insightsEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationsEnabled', 'insightsConfiguration_notificationsEnabled' - Set the NotificationsEnabled value to true to enable insights
-- notifications. Notifications can only be enabled on a group with
-- InsightsEnabled set to true.
--
-- 'insightsEnabled', 'insightsConfiguration_insightsEnabled' - Set the InsightsEnabled value to true to enable insights or false to
-- disable insights.
newInsightsConfiguration ::
  InsightsConfiguration
newInsightsConfiguration =
  InsightsConfiguration'
    { notificationsEnabled =
        Core.Nothing,
      insightsEnabled = Core.Nothing
    }

-- | Set the NotificationsEnabled value to true to enable insights
-- notifications. Notifications can only be enabled on a group with
-- InsightsEnabled set to true.
insightsConfiguration_notificationsEnabled :: Lens.Lens' InsightsConfiguration (Core.Maybe Core.Bool)
insightsConfiguration_notificationsEnabled = Lens.lens (\InsightsConfiguration' {notificationsEnabled} -> notificationsEnabled) (\s@InsightsConfiguration' {} a -> s {notificationsEnabled = a} :: InsightsConfiguration)

-- | Set the InsightsEnabled value to true to enable insights or false to
-- disable insights.
insightsConfiguration_insightsEnabled :: Lens.Lens' InsightsConfiguration (Core.Maybe Core.Bool)
insightsConfiguration_insightsEnabled = Lens.lens (\InsightsConfiguration' {insightsEnabled} -> insightsEnabled) (\s@InsightsConfiguration' {} a -> s {insightsEnabled = a} :: InsightsConfiguration)

instance Core.FromJSON InsightsConfiguration where
  parseJSON =
    Core.withObject
      "InsightsConfiguration"
      ( \x ->
          InsightsConfiguration'
            Core.<$> (x Core..:? "NotificationsEnabled")
            Core.<*> (x Core..:? "InsightsEnabled")
      )

instance Core.Hashable InsightsConfiguration

instance Core.NFData InsightsConfiguration

instance Core.ToJSON InsightsConfiguration where
  toJSON InsightsConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationsEnabled" Core..=)
              Core.<$> notificationsEnabled,
            ("InsightsEnabled" Core..=)
              Core.<$> insightsEnabled
          ]
      )

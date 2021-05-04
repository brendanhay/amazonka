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
-- Module      : Network.AWS.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightsConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The structure containing configurations related to insights.
--
-- /See:/ 'newInsightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { -- | Set the NotificationsEnabled value to true to enable insights
    -- notifications. Notifications can only be enabled on a group with
    -- InsightsEnabled set to true.
    notificationsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Set the InsightsEnabled value to true to enable insights or false to
    -- disable insights.
    insightsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      insightsEnabled = Prelude.Nothing
    }

-- | Set the NotificationsEnabled value to true to enable insights
-- notifications. Notifications can only be enabled on a group with
-- InsightsEnabled set to true.
insightsConfiguration_notificationsEnabled :: Lens.Lens' InsightsConfiguration (Prelude.Maybe Prelude.Bool)
insightsConfiguration_notificationsEnabled = Lens.lens (\InsightsConfiguration' {notificationsEnabled} -> notificationsEnabled) (\s@InsightsConfiguration' {} a -> s {notificationsEnabled = a} :: InsightsConfiguration)

-- | Set the InsightsEnabled value to true to enable insights or false to
-- disable insights.
insightsConfiguration_insightsEnabled :: Lens.Lens' InsightsConfiguration (Prelude.Maybe Prelude.Bool)
insightsConfiguration_insightsEnabled = Lens.lens (\InsightsConfiguration' {insightsEnabled} -> insightsEnabled) (\s@InsightsConfiguration' {} a -> s {insightsEnabled = a} :: InsightsConfiguration)

instance Prelude.FromJSON InsightsConfiguration where
  parseJSON =
    Prelude.withObject
      "InsightsConfiguration"
      ( \x ->
          InsightsConfiguration'
            Prelude.<$> (x Prelude..:? "NotificationsEnabled")
            Prelude.<*> (x Prelude..:? "InsightsEnabled")
      )

instance Prelude.Hashable InsightsConfiguration

instance Prelude.NFData InsightsConfiguration

instance Prelude.ToJSON InsightsConfiguration where
  toJSON InsightsConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationsEnabled" Prelude..=)
              Prelude.<$> notificationsEnabled,
            ("InsightsEnabled" Prelude..=)
              Prelude.<$> insightsEnabled
          ]
      )

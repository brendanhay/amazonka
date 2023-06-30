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
-- Module      : Amazonka.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InsightsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure containing configurations related to insights.
--
-- /See:/ 'newInsightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { -- | Set the InsightsEnabled value to true to enable insights or false to
    -- disable insights.
    insightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Set the NotificationsEnabled value to true to enable insights
    -- notifications. Notifications can only be enabled on a group with
    -- InsightsEnabled set to true.
    notificationsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsEnabled', 'insightsConfiguration_insightsEnabled' - Set the InsightsEnabled value to true to enable insights or false to
-- disable insights.
--
-- 'notificationsEnabled', 'insightsConfiguration_notificationsEnabled' - Set the NotificationsEnabled value to true to enable insights
-- notifications. Notifications can only be enabled on a group with
-- InsightsEnabled set to true.
newInsightsConfiguration ::
  InsightsConfiguration
newInsightsConfiguration =
  InsightsConfiguration'
    { insightsEnabled =
        Prelude.Nothing,
      notificationsEnabled = Prelude.Nothing
    }

-- | Set the InsightsEnabled value to true to enable insights or false to
-- disable insights.
insightsConfiguration_insightsEnabled :: Lens.Lens' InsightsConfiguration (Prelude.Maybe Prelude.Bool)
insightsConfiguration_insightsEnabled = Lens.lens (\InsightsConfiguration' {insightsEnabled} -> insightsEnabled) (\s@InsightsConfiguration' {} a -> s {insightsEnabled = a} :: InsightsConfiguration)

-- | Set the NotificationsEnabled value to true to enable insights
-- notifications. Notifications can only be enabled on a group with
-- InsightsEnabled set to true.
insightsConfiguration_notificationsEnabled :: Lens.Lens' InsightsConfiguration (Prelude.Maybe Prelude.Bool)
insightsConfiguration_notificationsEnabled = Lens.lens (\InsightsConfiguration' {notificationsEnabled} -> notificationsEnabled) (\s@InsightsConfiguration' {} a -> s {notificationsEnabled = a} :: InsightsConfiguration)

instance Data.FromJSON InsightsConfiguration where
  parseJSON =
    Data.withObject
      "InsightsConfiguration"
      ( \x ->
          InsightsConfiguration'
            Prelude.<$> (x Data..:? "InsightsEnabled")
            Prelude.<*> (x Data..:? "NotificationsEnabled")
      )

instance Prelude.Hashable InsightsConfiguration where
  hashWithSalt _salt InsightsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` insightsEnabled
      `Prelude.hashWithSalt` notificationsEnabled

instance Prelude.NFData InsightsConfiguration where
  rnf InsightsConfiguration' {..} =
    Prelude.rnf insightsEnabled
      `Prelude.seq` Prelude.rnf notificationsEnabled

instance Data.ToJSON InsightsConfiguration where
  toJSON InsightsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightsEnabled" Data..=)
              Prelude.<$> insightsEnabled,
            ("NotificationsEnabled" Data..=)
              Prelude.<$> notificationsEnabled
          ]
      )

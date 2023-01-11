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
-- Module      : Amazonka.SESV2.Types.DashboardOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DashboardOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.FeatureStatus

-- | An object containing additional settings for your VDM configuration as
-- applicable to the Dashboard.
--
-- /See:/ 'newDashboardOptions' smart constructor.
data DashboardOptions = DashboardOptions'
  { -- | Specifies the status of your VDM engagement metrics collection. Can be
    -- one of the following:
    --
    -- -   @ENABLED@ – Amazon SES enables engagement metrics for the
    --     configuration set.
    --
    -- -   @DISABLED@ – Amazon SES disables engagement metrics for the
    --     configuration set.
    engagementMetrics :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engagementMetrics', 'dashboardOptions_engagementMetrics' - Specifies the status of your VDM engagement metrics collection. Can be
-- one of the following:
--
-- -   @ENABLED@ – Amazon SES enables engagement metrics for the
--     configuration set.
--
-- -   @DISABLED@ – Amazon SES disables engagement metrics for the
--     configuration set.
newDashboardOptions ::
  DashboardOptions
newDashboardOptions =
  DashboardOptions'
    { engagementMetrics =
        Prelude.Nothing
    }

-- | Specifies the status of your VDM engagement metrics collection. Can be
-- one of the following:
--
-- -   @ENABLED@ – Amazon SES enables engagement metrics for the
--     configuration set.
--
-- -   @DISABLED@ – Amazon SES disables engagement metrics for the
--     configuration set.
dashboardOptions_engagementMetrics :: Lens.Lens' DashboardOptions (Prelude.Maybe FeatureStatus)
dashboardOptions_engagementMetrics = Lens.lens (\DashboardOptions' {engagementMetrics} -> engagementMetrics) (\s@DashboardOptions' {} a -> s {engagementMetrics = a} :: DashboardOptions)

instance Data.FromJSON DashboardOptions where
  parseJSON =
    Data.withObject
      "DashboardOptions"
      ( \x ->
          DashboardOptions'
            Prelude.<$> (x Data..:? "EngagementMetrics")
      )

instance Prelude.Hashable DashboardOptions where
  hashWithSalt _salt DashboardOptions' {..} =
    _salt `Prelude.hashWithSalt` engagementMetrics

instance Prelude.NFData DashboardOptions where
  rnf DashboardOptions' {..} =
    Prelude.rnf engagementMetrics

instance Data.ToJSON DashboardOptions where
  toJSON DashboardOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EngagementMetrics" Data..=)
              Prelude.<$> engagementMetrics
          ]
      )

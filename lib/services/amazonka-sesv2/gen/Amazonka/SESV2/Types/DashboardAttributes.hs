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
-- Module      : Amazonka.SESV2.Types.DashboardAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DashboardAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.FeatureStatus

-- | An object containing additional settings for your VDM configuration as
-- applicable to the Dashboard.
--
-- /See:/ 'newDashboardAttributes' smart constructor.
data DashboardAttributes = DashboardAttributes'
  { -- | Specifies the status of your VDM engagement metrics collection. Can be
    -- one of the following:
    --
    -- -   @ENABLED@ – Amazon SES enables engagement metrics for your account.
    --
    -- -   @DISABLED@ – Amazon SES disables engagement metrics for your
    --     account.
    engagementMetrics :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engagementMetrics', 'dashboardAttributes_engagementMetrics' - Specifies the status of your VDM engagement metrics collection. Can be
-- one of the following:
--
-- -   @ENABLED@ – Amazon SES enables engagement metrics for your account.
--
-- -   @DISABLED@ – Amazon SES disables engagement metrics for your
--     account.
newDashboardAttributes ::
  DashboardAttributes
newDashboardAttributes =
  DashboardAttributes'
    { engagementMetrics =
        Prelude.Nothing
    }

-- | Specifies the status of your VDM engagement metrics collection. Can be
-- one of the following:
--
-- -   @ENABLED@ – Amazon SES enables engagement metrics for your account.
--
-- -   @DISABLED@ – Amazon SES disables engagement metrics for your
--     account.
dashboardAttributes_engagementMetrics :: Lens.Lens' DashboardAttributes (Prelude.Maybe FeatureStatus)
dashboardAttributes_engagementMetrics = Lens.lens (\DashboardAttributes' {engagementMetrics} -> engagementMetrics) (\s@DashboardAttributes' {} a -> s {engagementMetrics = a} :: DashboardAttributes)

instance Data.FromJSON DashboardAttributes where
  parseJSON =
    Data.withObject
      "DashboardAttributes"
      ( \x ->
          DashboardAttributes'
            Prelude.<$> (x Data..:? "EngagementMetrics")
      )

instance Prelude.Hashable DashboardAttributes where
  hashWithSalt _salt DashboardAttributes' {..} =
    _salt `Prelude.hashWithSalt` engagementMetrics

instance Prelude.NFData DashboardAttributes where
  rnf DashboardAttributes' {..} =
    Prelude.rnf engagementMetrics

instance Data.ToJSON DashboardAttributes where
  toJSON DashboardAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EngagementMetrics" Data..=)
              Prelude.<$> engagementMetrics
          ]
      )

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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RegisteredUserDashboardFeatureConfigurations

-- | Information about the dashboard you want to embed.
--
-- /See:/ 'newRegisteredUserDashboardEmbeddingConfiguration' smart constructor.
data RegisteredUserDashboardEmbeddingConfiguration = RegisteredUserDashboardEmbeddingConfiguration'
  { -- | The feature configurations of an embbedded Amazon QuickSight dashboard.
    featureConfigurations :: Prelude.Maybe RegisteredUserDashboardFeatureConfigurations,
    -- | The dashboard ID for the dashboard that you want the user to see first.
    -- This ID is included in the output URL. When the URL in response is
    -- accessed, Amazon QuickSight renders this dashboard if the user has
    -- permissions to view it.
    --
    -- If the user does not have permission to view this dashboard, they see a
    -- permissions error message.
    initialDashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserDashboardEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureConfigurations', 'registeredUserDashboardEmbeddingConfiguration_featureConfigurations' - The feature configurations of an embbedded Amazon QuickSight dashboard.
--
-- 'initialDashboardId', 'registeredUserDashboardEmbeddingConfiguration_initialDashboardId' - The dashboard ID for the dashboard that you want the user to see first.
-- This ID is included in the output URL. When the URL in response is
-- accessed, Amazon QuickSight renders this dashboard if the user has
-- permissions to view it.
--
-- If the user does not have permission to view this dashboard, they see a
-- permissions error message.
newRegisteredUserDashboardEmbeddingConfiguration ::
  -- | 'initialDashboardId'
  Prelude.Text ->
  RegisteredUserDashboardEmbeddingConfiguration
newRegisteredUserDashboardEmbeddingConfiguration
  pInitialDashboardId_ =
    RegisteredUserDashboardEmbeddingConfiguration'
      { featureConfigurations =
          Prelude.Nothing,
        initialDashboardId =
          pInitialDashboardId_
      }

-- | The feature configurations of an embbedded Amazon QuickSight dashboard.
registeredUserDashboardEmbeddingConfiguration_featureConfigurations :: Lens.Lens' RegisteredUserDashboardEmbeddingConfiguration (Prelude.Maybe RegisteredUserDashboardFeatureConfigurations)
registeredUserDashboardEmbeddingConfiguration_featureConfigurations = Lens.lens (\RegisteredUserDashboardEmbeddingConfiguration' {featureConfigurations} -> featureConfigurations) (\s@RegisteredUserDashboardEmbeddingConfiguration' {} a -> s {featureConfigurations = a} :: RegisteredUserDashboardEmbeddingConfiguration)

-- | The dashboard ID for the dashboard that you want the user to see first.
-- This ID is included in the output URL. When the URL in response is
-- accessed, Amazon QuickSight renders this dashboard if the user has
-- permissions to view it.
--
-- If the user does not have permission to view this dashboard, they see a
-- permissions error message.
registeredUserDashboardEmbeddingConfiguration_initialDashboardId :: Lens.Lens' RegisteredUserDashboardEmbeddingConfiguration Prelude.Text
registeredUserDashboardEmbeddingConfiguration_initialDashboardId = Lens.lens (\RegisteredUserDashboardEmbeddingConfiguration' {initialDashboardId} -> initialDashboardId) (\s@RegisteredUserDashboardEmbeddingConfiguration' {} a -> s {initialDashboardId = a} :: RegisteredUserDashboardEmbeddingConfiguration)

instance
  Prelude.Hashable
    RegisteredUserDashboardEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    RegisteredUserDashboardEmbeddingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` featureConfigurations
        `Prelude.hashWithSalt` initialDashboardId

instance
  Prelude.NFData
    RegisteredUserDashboardEmbeddingConfiguration
  where
  rnf
    RegisteredUserDashboardEmbeddingConfiguration' {..} =
      Prelude.rnf featureConfigurations
        `Prelude.seq` Prelude.rnf initialDashboardId

instance
  Data.ToJSON
    RegisteredUserDashboardEmbeddingConfiguration
  where
  toJSON
    RegisteredUserDashboardEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FeatureConfigurations" Data..=)
                Prelude.<$> featureConfigurations,
              Prelude.Just
                ("InitialDashboardId" Data..= initialDashboardId)
            ]
        )

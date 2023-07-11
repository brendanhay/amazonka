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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the dashboard you want to embed.
--
-- /See:/ 'newRegisteredUserDashboardEmbeddingConfiguration' smart constructor.
data RegisteredUserDashboardEmbeddingConfiguration = RegisteredUserDashboardEmbeddingConfiguration'
  { -- | The dashboard ID for the dashboard that you want the user to see first.
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
      { initialDashboardId =
          pInitialDashboardId_
      }

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
      _salt `Prelude.hashWithSalt` initialDashboardId

instance
  Prelude.NFData
    RegisteredUserDashboardEmbeddingConfiguration
  where
  rnf
    RegisteredUserDashboardEmbeddingConfiguration' {..} =
      Prelude.rnf initialDashboardId

instance
  Data.ToJSON
    RegisteredUserDashboardEmbeddingConfiguration
  where
  toJSON
    RegisteredUserDashboardEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("InitialDashboardId" Data..= initialDashboardId)
            ]
        )

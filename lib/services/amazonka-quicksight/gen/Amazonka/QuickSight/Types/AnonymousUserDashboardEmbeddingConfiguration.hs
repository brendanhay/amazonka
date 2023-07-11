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
-- Module      : Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the dashboard that you want to embed.
--
-- /See:/ 'newAnonymousUserDashboardEmbeddingConfiguration' smart constructor.
data AnonymousUserDashboardEmbeddingConfiguration = AnonymousUserDashboardEmbeddingConfiguration'
  { -- | The dashboard ID for the dashboard that you want the user to see first.
    -- This ID is included in the output URL. When the URL in response is
    -- accessed, Amazon QuickSight renders this dashboard.
    --
    -- The Amazon Resource Name (ARN) of this dashboard must be included in the
    -- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
    -- with @InvalidParameterValueException@.
    initialDashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnonymousUserDashboardEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialDashboardId', 'anonymousUserDashboardEmbeddingConfiguration_initialDashboardId' - The dashboard ID for the dashboard that you want the user to see first.
-- This ID is included in the output URL. When the URL in response is
-- accessed, Amazon QuickSight renders this dashboard.
--
-- The Amazon Resource Name (ARN) of this dashboard must be included in the
-- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
-- with @InvalidParameterValueException@.
newAnonymousUserDashboardEmbeddingConfiguration ::
  -- | 'initialDashboardId'
  Prelude.Text ->
  AnonymousUserDashboardEmbeddingConfiguration
newAnonymousUserDashboardEmbeddingConfiguration
  pInitialDashboardId_ =
    AnonymousUserDashboardEmbeddingConfiguration'
      { initialDashboardId =
          pInitialDashboardId_
      }

-- | The dashboard ID for the dashboard that you want the user to see first.
-- This ID is included in the output URL. When the URL in response is
-- accessed, Amazon QuickSight renders this dashboard.
--
-- The Amazon Resource Name (ARN) of this dashboard must be included in the
-- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
-- with @InvalidParameterValueException@.
anonymousUserDashboardEmbeddingConfiguration_initialDashboardId :: Lens.Lens' AnonymousUserDashboardEmbeddingConfiguration Prelude.Text
anonymousUserDashboardEmbeddingConfiguration_initialDashboardId = Lens.lens (\AnonymousUserDashboardEmbeddingConfiguration' {initialDashboardId} -> initialDashboardId) (\s@AnonymousUserDashboardEmbeddingConfiguration' {} a -> s {initialDashboardId = a} :: AnonymousUserDashboardEmbeddingConfiguration)

instance
  Prelude.Hashable
    AnonymousUserDashboardEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    AnonymousUserDashboardEmbeddingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` initialDashboardId

instance
  Prelude.NFData
    AnonymousUserDashboardEmbeddingConfiguration
  where
  rnf AnonymousUserDashboardEmbeddingConfiguration' {..} =
    Prelude.rnf initialDashboardId

instance
  Data.ToJSON
    AnonymousUserDashboardEmbeddingConfiguration
  where
  toJSON
    AnonymousUserDashboardEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("InitialDashboardId" Data..= initialDashboardId)
            ]
        )

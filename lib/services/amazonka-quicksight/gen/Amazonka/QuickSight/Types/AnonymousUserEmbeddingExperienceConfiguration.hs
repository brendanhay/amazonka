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
-- Module      : Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration

-- | The type of experience you want to embed. For anonymous users, you can
-- embed an Amazon QuickSight dashboard.
--
-- /See:/ 'newAnonymousUserEmbeddingExperienceConfiguration' smart constructor.
data AnonymousUserEmbeddingExperienceConfiguration = AnonymousUserEmbeddingExperienceConfiguration'
  { -- | The type of embedding experience. In this case, an Amazon QuickSight
    -- dashboard.
    dashboard :: Prelude.Maybe AnonymousUserDashboardEmbeddingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnonymousUserEmbeddingExperienceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboard', 'anonymousUserEmbeddingExperienceConfiguration_dashboard' - The type of embedding experience. In this case, an Amazon QuickSight
-- dashboard.
newAnonymousUserEmbeddingExperienceConfiguration ::
  AnonymousUserEmbeddingExperienceConfiguration
newAnonymousUserEmbeddingExperienceConfiguration =
  AnonymousUserEmbeddingExperienceConfiguration'
    { dashboard =
        Prelude.Nothing
    }

-- | The type of embedding experience. In this case, an Amazon QuickSight
-- dashboard.
anonymousUserEmbeddingExperienceConfiguration_dashboard :: Lens.Lens' AnonymousUserEmbeddingExperienceConfiguration (Prelude.Maybe AnonymousUserDashboardEmbeddingConfiguration)
anonymousUserEmbeddingExperienceConfiguration_dashboard = Lens.lens (\AnonymousUserEmbeddingExperienceConfiguration' {dashboard} -> dashboard) (\s@AnonymousUserEmbeddingExperienceConfiguration' {} a -> s {dashboard = a} :: AnonymousUserEmbeddingExperienceConfiguration)

instance
  Prelude.Hashable
    AnonymousUserEmbeddingExperienceConfiguration
  where
  hashWithSalt
    _salt
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` dashboard

instance
  Prelude.NFData
    AnonymousUserEmbeddingExperienceConfiguration
  where
  rnf
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      Prelude.rnf dashboard

instance
  Core.ToJSON
    AnonymousUserEmbeddingExperienceConfiguration
  where
  toJSON
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Dashboard" Core..=) Prelude.<$> dashboard]
        )

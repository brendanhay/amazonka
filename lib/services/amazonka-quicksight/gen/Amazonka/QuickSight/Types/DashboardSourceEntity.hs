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
-- Module      : Amazonka.QuickSight.Types.DashboardSourceEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardSourceEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardSourceTemplate

-- | Dashboard source entity.
--
-- /See:/ 'newDashboardSourceEntity' smart constructor.
data DashboardSourceEntity = DashboardSourceEntity'
  { -- | Source template.
    sourceTemplate :: Prelude.Maybe DashboardSourceTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardSourceEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTemplate', 'dashboardSourceEntity_sourceTemplate' - Source template.
newDashboardSourceEntity ::
  DashboardSourceEntity
newDashboardSourceEntity =
  DashboardSourceEntity'
    { sourceTemplate =
        Prelude.Nothing
    }

-- | Source template.
dashboardSourceEntity_sourceTemplate :: Lens.Lens' DashboardSourceEntity (Prelude.Maybe DashboardSourceTemplate)
dashboardSourceEntity_sourceTemplate = Lens.lens (\DashboardSourceEntity' {sourceTemplate} -> sourceTemplate) (\s@DashboardSourceEntity' {} a -> s {sourceTemplate = a} :: DashboardSourceEntity)

instance Prelude.Hashable DashboardSourceEntity where
  hashWithSalt _salt DashboardSourceEntity' {..} =
    _salt `Prelude.hashWithSalt` sourceTemplate

instance Prelude.NFData DashboardSourceEntity where
  rnf DashboardSourceEntity' {..} =
    Prelude.rnf sourceTemplate

instance Data.ToJSON DashboardSourceEntity where
  toJSON DashboardSourceEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceTemplate" Data..=)
              Prelude.<$> sourceTemplate
          ]
      )

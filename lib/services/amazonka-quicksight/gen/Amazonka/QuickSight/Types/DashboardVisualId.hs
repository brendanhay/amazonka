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
-- Module      : Amazonka.QuickSight.Types.DashboardVisualId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVisualId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the following elements:
--
-- -   The @DashboardId@ of the dashboard that has the visual that you want
--     to embed.
--
-- -   The @SheetId@ of the sheet that has the visual that you want to
--     embed.
--
-- -   The @VisualId@ of the visual that you want to embed.
--
-- The @DashboardId@, @SheetId@, and @VisualId@ can be found in the
-- @IDs for developers@ section of the @Embed visual@ pane of the visual\'s
-- on-visual menu of the Amazon QuickSight console. You can also get the
-- @DashboardId@ with a @ListDashboards@ API operation.
--
-- /See:/ 'newDashboardVisualId' smart constructor.
data DashboardVisualId = DashboardVisualId'
  { -- | The ID of the dashboard that has the visual that you want to embed. The
    -- @DashboardId@ can be found in the @IDs for developers@ section of the
    -- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
    -- QuickSight console. You can also get the @DashboardId@ with a
    -- @ListDashboards@ API operation.
    dashboardId :: Prelude.Text,
    -- | The ID of the sheet that the has visual that you want to embed. The
    -- @SheetId@ can be found in the @IDs for developers@ section of the
    -- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
    -- QuickSight console.
    sheetId :: Prelude.Text,
    -- | The ID of the visual that you want to embed. The @VisualID@ can be found
    -- in the @IDs for developers@ section of the @Embed visual@ pane of the
    -- visual\'s on-visual menu of the Amazon QuickSight console.
    visualId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardVisualId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardId', 'dashboardVisualId_dashboardId' - The ID of the dashboard that has the visual that you want to embed. The
-- @DashboardId@ can be found in the @IDs for developers@ section of the
-- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
-- QuickSight console. You can also get the @DashboardId@ with a
-- @ListDashboards@ API operation.
--
-- 'sheetId', 'dashboardVisualId_sheetId' - The ID of the sheet that the has visual that you want to embed. The
-- @SheetId@ can be found in the @IDs for developers@ section of the
-- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
-- QuickSight console.
--
-- 'visualId', 'dashboardVisualId_visualId' - The ID of the visual that you want to embed. The @VisualID@ can be found
-- in the @IDs for developers@ section of the @Embed visual@ pane of the
-- visual\'s on-visual menu of the Amazon QuickSight console.
newDashboardVisualId ::
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'sheetId'
  Prelude.Text ->
  -- | 'visualId'
  Prelude.Text ->
  DashboardVisualId
newDashboardVisualId
  pDashboardId_
  pSheetId_
  pVisualId_ =
    DashboardVisualId'
      { dashboardId = pDashboardId_,
        sheetId = pSheetId_,
        visualId = pVisualId_
      }

-- | The ID of the dashboard that has the visual that you want to embed. The
-- @DashboardId@ can be found in the @IDs for developers@ section of the
-- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
-- QuickSight console. You can also get the @DashboardId@ with a
-- @ListDashboards@ API operation.
dashboardVisualId_dashboardId :: Lens.Lens' DashboardVisualId Prelude.Text
dashboardVisualId_dashboardId = Lens.lens (\DashboardVisualId' {dashboardId} -> dashboardId) (\s@DashboardVisualId' {} a -> s {dashboardId = a} :: DashboardVisualId)

-- | The ID of the sheet that the has visual that you want to embed. The
-- @SheetId@ can be found in the @IDs for developers@ section of the
-- @Embed visual@ pane of the visual\'s on-visual menu of the Amazon
-- QuickSight console.
dashboardVisualId_sheetId :: Lens.Lens' DashboardVisualId Prelude.Text
dashboardVisualId_sheetId = Lens.lens (\DashboardVisualId' {sheetId} -> sheetId) (\s@DashboardVisualId' {} a -> s {sheetId = a} :: DashboardVisualId)

-- | The ID of the visual that you want to embed. The @VisualID@ can be found
-- in the @IDs for developers@ section of the @Embed visual@ pane of the
-- visual\'s on-visual menu of the Amazon QuickSight console.
dashboardVisualId_visualId :: Lens.Lens' DashboardVisualId Prelude.Text
dashboardVisualId_visualId = Lens.lens (\DashboardVisualId' {visualId} -> visualId) (\s@DashboardVisualId' {} a -> s {visualId = a} :: DashboardVisualId)

instance Prelude.Hashable DashboardVisualId where
  hashWithSalt _salt DashboardVisualId' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` sheetId
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData DashboardVisualId where
  rnf DashboardVisualId' {..} =
    Prelude.rnf dashboardId `Prelude.seq`
      Prelude.rnf sheetId `Prelude.seq`
        Prelude.rnf visualId

instance Data.ToJSON DashboardVisualId where
  toJSON DashboardVisualId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DashboardId" Data..= dashboardId),
            Prelude.Just ("SheetId" Data..= sheetId),
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )

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
-- Module      : Amazonka.ConnectCases.Types.BasicLayout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.BasicLayout where

import Amazonka.ConnectCases.Types.LayoutSections
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Content specific to @BasicLayout@ type. It configures fields in the top
-- panel and More Info tab of agent application.
--
-- /See:/ 'newBasicLayout' smart constructor.
data BasicLayout = BasicLayout'
  { -- | This represents sections in a panel of the page layout.
    topPanel :: Prelude.Maybe LayoutSections,
    -- | This represents sections in a tab of the page layout.
    moreInfo :: Prelude.Maybe LayoutSections
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasicLayout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topPanel', 'basicLayout_topPanel' - This represents sections in a panel of the page layout.
--
-- 'moreInfo', 'basicLayout_moreInfo' - This represents sections in a tab of the page layout.
newBasicLayout ::
  BasicLayout
newBasicLayout =
  BasicLayout'
    { topPanel = Prelude.Nothing,
      moreInfo = Prelude.Nothing
    }

-- | This represents sections in a panel of the page layout.
basicLayout_topPanel :: Lens.Lens' BasicLayout (Prelude.Maybe LayoutSections)
basicLayout_topPanel = Lens.lens (\BasicLayout' {topPanel} -> topPanel) (\s@BasicLayout' {} a -> s {topPanel = a} :: BasicLayout)

-- | This represents sections in a tab of the page layout.
basicLayout_moreInfo :: Lens.Lens' BasicLayout (Prelude.Maybe LayoutSections)
basicLayout_moreInfo = Lens.lens (\BasicLayout' {moreInfo} -> moreInfo) (\s@BasicLayout' {} a -> s {moreInfo = a} :: BasicLayout)

instance Core.FromJSON BasicLayout where
  parseJSON =
    Core.withObject
      "BasicLayout"
      ( \x ->
          BasicLayout'
            Prelude.<$> (x Core..:? "topPanel")
            Prelude.<*> (x Core..:? "moreInfo")
      )

instance Prelude.Hashable BasicLayout where
  hashWithSalt _salt BasicLayout' {..} =
    _salt `Prelude.hashWithSalt` topPanel
      `Prelude.hashWithSalt` moreInfo

instance Prelude.NFData BasicLayout where
  rnf BasicLayout' {..} =
    Prelude.rnf topPanel
      `Prelude.seq` Prelude.rnf moreInfo

instance Core.ToJSON BasicLayout where
  toJSON BasicLayout' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("topPanel" Core..=) Prelude.<$> topPanel,
            ("moreInfo" Core..=) Prelude.<$> moreInfo
          ]
      )

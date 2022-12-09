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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Content specific to @BasicLayout@ type. It configures fields in the top
-- panel and More Info tab of agent application.
--
-- /See:/ 'newBasicLayout' smart constructor.
data BasicLayout = BasicLayout'
  { -- | This represents sections in a tab of the page layout.
    moreInfo :: Prelude.Maybe LayoutSections,
    -- | This represents sections in a panel of the page layout.
    topPanel :: Prelude.Maybe LayoutSections
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
-- 'moreInfo', 'basicLayout_moreInfo' - This represents sections in a tab of the page layout.
--
-- 'topPanel', 'basicLayout_topPanel' - This represents sections in a panel of the page layout.
newBasicLayout ::
  BasicLayout
newBasicLayout =
  BasicLayout'
    { moreInfo = Prelude.Nothing,
      topPanel = Prelude.Nothing
    }

-- | This represents sections in a tab of the page layout.
basicLayout_moreInfo :: Lens.Lens' BasicLayout (Prelude.Maybe LayoutSections)
basicLayout_moreInfo = Lens.lens (\BasicLayout' {moreInfo} -> moreInfo) (\s@BasicLayout' {} a -> s {moreInfo = a} :: BasicLayout)

-- | This represents sections in a panel of the page layout.
basicLayout_topPanel :: Lens.Lens' BasicLayout (Prelude.Maybe LayoutSections)
basicLayout_topPanel = Lens.lens (\BasicLayout' {topPanel} -> topPanel) (\s@BasicLayout' {} a -> s {topPanel = a} :: BasicLayout)

instance Data.FromJSON BasicLayout where
  parseJSON =
    Data.withObject
      "BasicLayout"
      ( \x ->
          BasicLayout'
            Prelude.<$> (x Data..:? "moreInfo")
            Prelude.<*> (x Data..:? "topPanel")
      )

instance Prelude.Hashable BasicLayout where
  hashWithSalt _salt BasicLayout' {..} =
    _salt `Prelude.hashWithSalt` moreInfo
      `Prelude.hashWithSalt` topPanel

instance Prelude.NFData BasicLayout where
  rnf BasicLayout' {..} =
    Prelude.rnf moreInfo
      `Prelude.seq` Prelude.rnf topPanel

instance Data.ToJSON BasicLayout where
  toJSON BasicLayout' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("moreInfo" Data..=) Prelude.<$> moreInfo,
            ("topPanel" Data..=) Prelude.<$> topPanel
          ]
      )

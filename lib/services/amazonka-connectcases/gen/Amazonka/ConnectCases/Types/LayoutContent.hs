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
-- Module      : Amazonka.ConnectCases.Types.LayoutContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.LayoutContent where

import Amazonka.ConnectCases.Types.BasicLayout
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object to store union of different versions of layout content.
--
-- /See:/ 'newLayoutContent' smart constructor.
data LayoutContent = LayoutContent'
  { -- | Content specific to @BasicLayout@ type. It configures fields in the top
    -- panel and More Info tab of Cases user interface.
    basic :: Prelude.Maybe BasicLayout
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayoutContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basic', 'layoutContent_basic' - Content specific to @BasicLayout@ type. It configures fields in the top
-- panel and More Info tab of Cases user interface.
newLayoutContent ::
  LayoutContent
newLayoutContent =
  LayoutContent' {basic = Prelude.Nothing}

-- | Content specific to @BasicLayout@ type. It configures fields in the top
-- panel and More Info tab of Cases user interface.
layoutContent_basic :: Lens.Lens' LayoutContent (Prelude.Maybe BasicLayout)
layoutContent_basic = Lens.lens (\LayoutContent' {basic} -> basic) (\s@LayoutContent' {} a -> s {basic = a} :: LayoutContent)

instance Data.FromJSON LayoutContent where
  parseJSON =
    Data.withObject
      "LayoutContent"
      ( \x ->
          LayoutContent' Prelude.<$> (x Data..:? "basic")
      )

instance Prelude.Hashable LayoutContent where
  hashWithSalt _salt LayoutContent' {..} =
    _salt `Prelude.hashWithSalt` basic

instance Prelude.NFData LayoutContent where
  rnf LayoutContent' {..} = Prelude.rnf basic

instance Data.ToJSON LayoutContent where
  toJSON LayoutContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [("basic" Data..=) Prelude.<$> basic]
      )

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
-- Module      : Amazonka.ConnectCases.Types.LayoutSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.LayoutSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for the summarized details of the layout.
--
-- /See:/ 'newLayoutSummary' smart constructor.
data LayoutSummary = LayoutSummary'
  { -- | The Amazon Resource Name (ARN) of the layout.
    layoutArn :: Prelude.Text,
    -- | The unique identifier for of the layout.
    layoutId :: Prelude.Text,
    -- | The name of the layout.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayoutSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layoutArn', 'layoutSummary_layoutArn' - The Amazon Resource Name (ARN) of the layout.
--
-- 'layoutId', 'layoutSummary_layoutId' - The unique identifier for of the layout.
--
-- 'name', 'layoutSummary_name' - The name of the layout.
newLayoutSummary ::
  -- | 'layoutArn'
  Prelude.Text ->
  -- | 'layoutId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  LayoutSummary
newLayoutSummary pLayoutArn_ pLayoutId_ pName_ =
  LayoutSummary'
    { layoutArn = pLayoutArn_,
      layoutId = pLayoutId_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the layout.
layoutSummary_layoutArn :: Lens.Lens' LayoutSummary Prelude.Text
layoutSummary_layoutArn = Lens.lens (\LayoutSummary' {layoutArn} -> layoutArn) (\s@LayoutSummary' {} a -> s {layoutArn = a} :: LayoutSummary)

-- | The unique identifier for of the layout.
layoutSummary_layoutId :: Lens.Lens' LayoutSummary Prelude.Text
layoutSummary_layoutId = Lens.lens (\LayoutSummary' {layoutId} -> layoutId) (\s@LayoutSummary' {} a -> s {layoutId = a} :: LayoutSummary)

-- | The name of the layout.
layoutSummary_name :: Lens.Lens' LayoutSummary Prelude.Text
layoutSummary_name = Lens.lens (\LayoutSummary' {name} -> name) (\s@LayoutSummary' {} a -> s {name = a} :: LayoutSummary)

instance Data.FromJSON LayoutSummary where
  parseJSON =
    Data.withObject
      "LayoutSummary"
      ( \x ->
          LayoutSummary'
            Prelude.<$> (x Data..: "layoutArn")
            Prelude.<*> (x Data..: "layoutId")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable LayoutSummary where
  hashWithSalt _salt LayoutSummary' {..} =
    _salt `Prelude.hashWithSalt` layoutArn
      `Prelude.hashWithSalt` layoutId
      `Prelude.hashWithSalt` name

instance Prelude.NFData LayoutSummary where
  rnf LayoutSummary' {..} =
    Prelude.rnf layoutArn
      `Prelude.seq` Prelude.rnf layoutId
      `Prelude.seq` Prelude.rnf name

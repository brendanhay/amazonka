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
-- Module      : Network.AWS.SSM.Types.OpsEntityItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntityItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The OpsItem summaries result item.
--
-- /See:/ 'newOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The time OpsItem data was captured.
    captureTime :: Core.Maybe Core.Text,
    -- | The detailed data content for an OpsItem summaries result item.
    content :: Core.Maybe [Core.HashMap Core.Text Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsEntityItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureTime', 'opsEntityItem_captureTime' - The time OpsItem data was captured.
--
-- 'content', 'opsEntityItem_content' - The detailed data content for an OpsItem summaries result item.
newOpsEntityItem ::
  OpsEntityItem
newOpsEntityItem =
  OpsEntityItem'
    { captureTime = Core.Nothing,
      content = Core.Nothing
    }

-- | The time OpsItem data was captured.
opsEntityItem_captureTime :: Lens.Lens' OpsEntityItem (Core.Maybe Core.Text)
opsEntityItem_captureTime = Lens.lens (\OpsEntityItem' {captureTime} -> captureTime) (\s@OpsEntityItem' {} a -> s {captureTime = a} :: OpsEntityItem)

-- | The detailed data content for an OpsItem summaries result item.
opsEntityItem_content :: Lens.Lens' OpsEntityItem (Core.Maybe [Core.HashMap Core.Text Core.Text])
opsEntityItem_content = Lens.lens (\OpsEntityItem' {content} -> content) (\s@OpsEntityItem' {} a -> s {content = a} :: OpsEntityItem) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON OpsEntityItem where
  parseJSON =
    Core.withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            Core.<$> (x Core..:? "CaptureTime")
            Core.<*> (x Core..:? "Content" Core..!= Core.mempty)
      )

instance Core.Hashable OpsEntityItem

instance Core.NFData OpsEntityItem

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
import qualified Network.AWS.Prelude as Prelude

-- | The OpsItem summaries result item.
--
-- /See:/ 'newOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The time OpsItem data was captured.
    captureTime :: Prelude.Maybe Prelude.Text,
    -- | The detailed data content for an OpsItem summaries result item.
    content :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { captureTime = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The time OpsItem data was captured.
opsEntityItem_captureTime :: Lens.Lens' OpsEntityItem (Prelude.Maybe Prelude.Text)
opsEntityItem_captureTime = Lens.lens (\OpsEntityItem' {captureTime} -> captureTime) (\s@OpsEntityItem' {} a -> s {captureTime = a} :: OpsEntityItem)

-- | The detailed data content for an OpsItem summaries result item.
opsEntityItem_content :: Lens.Lens' OpsEntityItem (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
opsEntityItem_content = Lens.lens (\OpsEntityItem' {content} -> content) (\s@OpsEntityItem' {} a -> s {content = a} :: OpsEntityItem) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON OpsEntityItem where
  parseJSON =
    Core.withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            Prelude.<$> (x Core..:? "CaptureTime")
            Prelude.<*> (x Core..:? "Content" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable OpsEntityItem

instance Prelude.NFData OpsEntityItem

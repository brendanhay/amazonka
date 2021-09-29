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

-- | The OpsData summary.
--
-- /See:/ 'newOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The details of an OpsData summary.
    content :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The time the OpsData was captured.
    captureTime :: Prelude.Maybe Prelude.Text
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
-- 'content', 'opsEntityItem_content' - The details of an OpsData summary.
--
-- 'captureTime', 'opsEntityItem_captureTime' - The time the OpsData was captured.
newOpsEntityItem ::
  OpsEntityItem
newOpsEntityItem =
  OpsEntityItem'
    { content = Prelude.Nothing,
      captureTime = Prelude.Nothing
    }

-- | The details of an OpsData summary.
opsEntityItem_content :: Lens.Lens' OpsEntityItem (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
opsEntityItem_content = Lens.lens (\OpsEntityItem' {content} -> content) (\s@OpsEntityItem' {} a -> s {content = a} :: OpsEntityItem) Prelude.. Lens.mapping Lens._Coerce

-- | The time the OpsData was captured.
opsEntityItem_captureTime :: Lens.Lens' OpsEntityItem (Prelude.Maybe Prelude.Text)
opsEntityItem_captureTime = Lens.lens (\OpsEntityItem' {captureTime} -> captureTime) (\s@OpsEntityItem' {} a -> s {captureTime = a} :: OpsEntityItem)

instance Core.FromJSON OpsEntityItem where
  parseJSON =
    Core.withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            Prelude.<$> (x Core..:? "Content" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CaptureTime")
      )

instance Prelude.Hashable OpsEntityItem

instance Prelude.NFData OpsEntityItem

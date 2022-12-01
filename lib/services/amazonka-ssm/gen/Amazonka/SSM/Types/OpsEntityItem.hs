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
-- Module      : Amazonka.SSM.Types.OpsEntityItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsEntityItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The OpsData summary.
--
-- /See:/ 'newOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The time the OpsData was captured.
    captureTime :: Prelude.Maybe Prelude.Text,
    -- | The details of an OpsData summary.
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
-- 'captureTime', 'opsEntityItem_captureTime' - The time the OpsData was captured.
--
-- 'content', 'opsEntityItem_content' - The details of an OpsData summary.
newOpsEntityItem ::
  OpsEntityItem
newOpsEntityItem =
  OpsEntityItem'
    { captureTime = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The time the OpsData was captured.
opsEntityItem_captureTime :: Lens.Lens' OpsEntityItem (Prelude.Maybe Prelude.Text)
opsEntityItem_captureTime = Lens.lens (\OpsEntityItem' {captureTime} -> captureTime) (\s@OpsEntityItem' {} a -> s {captureTime = a} :: OpsEntityItem)

-- | The details of an OpsData summary.
opsEntityItem_content :: Lens.Lens' OpsEntityItem (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
opsEntityItem_content = Lens.lens (\OpsEntityItem' {content} -> content) (\s@OpsEntityItem' {} a -> s {content = a} :: OpsEntityItem) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON OpsEntityItem where
  parseJSON =
    Core.withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            Prelude.<$> (x Core..:? "CaptureTime")
            Prelude.<*> (x Core..:? "Content" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable OpsEntityItem where
  hashWithSalt _salt OpsEntityItem' {..} =
    _salt `Prelude.hashWithSalt` captureTime
      `Prelude.hashWithSalt` content

instance Prelude.NFData OpsEntityItem where
  rnf OpsEntityItem' {..} =
    Prelude.rnf captureTime
      `Prelude.seq` Prelude.rnf content

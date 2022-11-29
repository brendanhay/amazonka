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
-- Module      : Amazonka.Rekognition.Types.LabelCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LabelCategory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The category that applies to a given label.
--
-- /See:/ 'newLabelCategory' smart constructor.
data LabelCategory = LabelCategory'
  { -- | The name of a category that applies to a given label.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'labelCategory_name' - The name of a category that applies to a given label.
newLabelCategory ::
  LabelCategory
newLabelCategory =
  LabelCategory' {name = Prelude.Nothing}

-- | The name of a category that applies to a given label.
labelCategory_name :: Lens.Lens' LabelCategory (Prelude.Maybe Prelude.Text)
labelCategory_name = Lens.lens (\LabelCategory' {name} -> name) (\s@LabelCategory' {} a -> s {name = a} :: LabelCategory)

instance Core.FromJSON LabelCategory where
  parseJSON =
    Core.withObject
      "LabelCategory"
      ( \x ->
          LabelCategory' Prelude.<$> (x Core..:? "Name")
      )

instance Prelude.Hashable LabelCategory where
  hashWithSalt _salt LabelCategory' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData LabelCategory where
  rnf LabelCategory' {..} = Prelude.rnf name

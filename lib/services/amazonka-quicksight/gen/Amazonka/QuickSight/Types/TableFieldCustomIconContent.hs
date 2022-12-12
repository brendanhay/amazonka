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
-- Module      : Amazonka.QuickSight.Types.TableFieldCustomIconContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldCustomIconContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldIconSetType

-- | The custom icon content for the table link content configuration.
--
-- /See:/ 'newTableFieldCustomIconContent' smart constructor.
data TableFieldCustomIconContent = TableFieldCustomIconContent'
  { -- | The icon set type (link) of the custom icon content for table URL link
    -- content.
    icon :: Prelude.Maybe TableFieldIconSetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldCustomIconContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'icon', 'tableFieldCustomIconContent_icon' - The icon set type (link) of the custom icon content for table URL link
-- content.
newTableFieldCustomIconContent ::
  TableFieldCustomIconContent
newTableFieldCustomIconContent =
  TableFieldCustomIconContent'
    { icon =
        Prelude.Nothing
    }

-- | The icon set type (link) of the custom icon content for table URL link
-- content.
tableFieldCustomIconContent_icon :: Lens.Lens' TableFieldCustomIconContent (Prelude.Maybe TableFieldIconSetType)
tableFieldCustomIconContent_icon = Lens.lens (\TableFieldCustomIconContent' {icon} -> icon) (\s@TableFieldCustomIconContent' {} a -> s {icon = a} :: TableFieldCustomIconContent)

instance Data.FromJSON TableFieldCustomIconContent where
  parseJSON =
    Data.withObject
      "TableFieldCustomIconContent"
      ( \x ->
          TableFieldCustomIconContent'
            Prelude.<$> (x Data..:? "Icon")
      )

instance Prelude.Hashable TableFieldCustomIconContent where
  hashWithSalt _salt TableFieldCustomIconContent' {..} =
    _salt `Prelude.hashWithSalt` icon

instance Prelude.NFData TableFieldCustomIconContent where
  rnf TableFieldCustomIconContent' {..} =
    Prelude.rnf icon

instance Data.ToJSON TableFieldCustomIconContent where
  toJSON TableFieldCustomIconContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Icon" Data..=) Prelude.<$> icon]
      )

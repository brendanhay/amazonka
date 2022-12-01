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
-- Module      : Amazonka.M2.Types.GdgAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.GdgAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The required attributes for a generation data group data set. A
-- generation data set is one of a collection of successive, historically
-- related, catalogued data sets that together are known as a generation
-- data group (GDG). Use this structure when you want to import a GDG. For
-- more information on GDG, see
-- <https://www.ibm.com/docs/en/zos/2.3.0?topic=guide-generation-data-sets Generation data sets>.
--
-- /See:/ 'newGdgAttributes' smart constructor.
data GdgAttributes = GdgAttributes'
  { -- | The maximum number of generation data sets, up to 255, in a GDG.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The disposition of the data set in the catalog.
    rollDisposition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GdgAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'gdgAttributes_limit' - The maximum number of generation data sets, up to 255, in a GDG.
--
-- 'rollDisposition', 'gdgAttributes_rollDisposition' - The disposition of the data set in the catalog.
newGdgAttributes ::
  GdgAttributes
newGdgAttributes =
  GdgAttributes'
    { limit = Prelude.Nothing,
      rollDisposition = Prelude.Nothing
    }

-- | The maximum number of generation data sets, up to 255, in a GDG.
gdgAttributes_limit :: Lens.Lens' GdgAttributes (Prelude.Maybe Prelude.Int)
gdgAttributes_limit = Lens.lens (\GdgAttributes' {limit} -> limit) (\s@GdgAttributes' {} a -> s {limit = a} :: GdgAttributes)

-- | The disposition of the data set in the catalog.
gdgAttributes_rollDisposition :: Lens.Lens' GdgAttributes (Prelude.Maybe Prelude.Text)
gdgAttributes_rollDisposition = Lens.lens (\GdgAttributes' {rollDisposition} -> rollDisposition) (\s@GdgAttributes' {} a -> s {rollDisposition = a} :: GdgAttributes)

instance Prelude.Hashable GdgAttributes where
  hashWithSalt _salt GdgAttributes' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` rollDisposition

instance Prelude.NFData GdgAttributes where
  rnf GdgAttributes' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf rollDisposition

instance Core.ToJSON GdgAttributes where
  toJSON GdgAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("limit" Core..=) Prelude.<$> limit,
            ("rollDisposition" Core..=)
              Prelude.<$> rollDisposition
          ]
      )

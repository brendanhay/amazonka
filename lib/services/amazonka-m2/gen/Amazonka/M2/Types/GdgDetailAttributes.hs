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
-- Module      : Amazonka.M2.Types.GdgDetailAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.GdgDetailAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The required attributes for a generation data group data set. A
-- generation data set is one of a collection of successive, historically
-- related, catalogued data sets that together are known as a generation
-- data group (GDG). Use this structure when you want to import a GDG. For
-- more information on GDG, see
-- <https://www.ibm.com/docs/en/zos/2.3.0?topic=guide-generation-data-sets Generation data sets>.
--
-- /See:/ 'newGdgDetailAttributes' smart constructor.
data GdgDetailAttributes = GdgDetailAttributes'
  { -- | The maximum number of generation data sets, up to 255, in a GDG.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The disposition of the data set in the catalog.
    rollDisposition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GdgDetailAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'gdgDetailAttributes_limit' - The maximum number of generation data sets, up to 255, in a GDG.
--
-- 'rollDisposition', 'gdgDetailAttributes_rollDisposition' - The disposition of the data set in the catalog.
newGdgDetailAttributes ::
  GdgDetailAttributes
newGdgDetailAttributes =
  GdgDetailAttributes'
    { limit = Prelude.Nothing,
      rollDisposition = Prelude.Nothing
    }

-- | The maximum number of generation data sets, up to 255, in a GDG.
gdgDetailAttributes_limit :: Lens.Lens' GdgDetailAttributes (Prelude.Maybe Prelude.Int)
gdgDetailAttributes_limit = Lens.lens (\GdgDetailAttributes' {limit} -> limit) (\s@GdgDetailAttributes' {} a -> s {limit = a} :: GdgDetailAttributes)

-- | The disposition of the data set in the catalog.
gdgDetailAttributes_rollDisposition :: Lens.Lens' GdgDetailAttributes (Prelude.Maybe Prelude.Text)
gdgDetailAttributes_rollDisposition = Lens.lens (\GdgDetailAttributes' {rollDisposition} -> rollDisposition) (\s@GdgDetailAttributes' {} a -> s {rollDisposition = a} :: GdgDetailAttributes)

instance Data.FromJSON GdgDetailAttributes where
  parseJSON =
    Data.withObject
      "GdgDetailAttributes"
      ( \x ->
          GdgDetailAttributes'
            Prelude.<$> (x Data..:? "limit")
            Prelude.<*> (x Data..:? "rollDisposition")
      )

instance Prelude.Hashable GdgDetailAttributes where
  hashWithSalt _salt GdgDetailAttributes' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` rollDisposition

instance Prelude.NFData GdgDetailAttributes where
  rnf GdgDetailAttributes' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf rollDisposition

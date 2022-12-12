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
-- Module      : Amazonka.Omics.Types.ListAnnotationStoresFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ListAnnotationStoresFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.StoreStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for annotation stores.
--
-- /See:/ 'newListAnnotationStoresFilter' smart constructor.
data ListAnnotationStoresFilter = ListAnnotationStoresFilter'
  { -- | A status to filter on.
    status :: Prelude.Maybe StoreStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationStoresFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listAnnotationStoresFilter_status' - A status to filter on.
newListAnnotationStoresFilter ::
  ListAnnotationStoresFilter
newListAnnotationStoresFilter =
  ListAnnotationStoresFilter'
    { status =
        Prelude.Nothing
    }

-- | A status to filter on.
listAnnotationStoresFilter_status :: Lens.Lens' ListAnnotationStoresFilter (Prelude.Maybe StoreStatus)
listAnnotationStoresFilter_status = Lens.lens (\ListAnnotationStoresFilter' {status} -> status) (\s@ListAnnotationStoresFilter' {} a -> s {status = a} :: ListAnnotationStoresFilter)

instance Prelude.Hashable ListAnnotationStoresFilter where
  hashWithSalt _salt ListAnnotationStoresFilter' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ListAnnotationStoresFilter where
  rnf ListAnnotationStoresFilter' {..} =
    Prelude.rnf status

instance Data.ToJSON ListAnnotationStoresFilter where
  toJSON ListAnnotationStoresFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("status" Data..=) Prelude.<$> status]
      )

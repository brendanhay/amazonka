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
-- Module      : Amazonka.LookoutEquipment.Types.CategoricalValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.CategoricalValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on categorical values in data.
--
-- /See:/ 'newCategoricalValues' smart constructor.
data CategoricalValues = CategoricalValues'
  { -- | Indicates the number of categories in the data.
    numberOfCategory :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether there is a potential data issue related to categorical
    -- values.
    status :: StatisticalIssueStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfCategory', 'categoricalValues_numberOfCategory' - Indicates the number of categories in the data.
--
-- 'status', 'categoricalValues_status' - Indicates whether there is a potential data issue related to categorical
-- values.
newCategoricalValues ::
  -- | 'status'
  StatisticalIssueStatus ->
  CategoricalValues
newCategoricalValues pStatus_ =
  CategoricalValues'
    { numberOfCategory =
        Prelude.Nothing,
      status = pStatus_
    }

-- | Indicates the number of categories in the data.
categoricalValues_numberOfCategory :: Lens.Lens' CategoricalValues (Prelude.Maybe Prelude.Int)
categoricalValues_numberOfCategory = Lens.lens (\CategoricalValues' {numberOfCategory} -> numberOfCategory) (\s@CategoricalValues' {} a -> s {numberOfCategory = a} :: CategoricalValues)

-- | Indicates whether there is a potential data issue related to categorical
-- values.
categoricalValues_status :: Lens.Lens' CategoricalValues StatisticalIssueStatus
categoricalValues_status = Lens.lens (\CategoricalValues' {status} -> status) (\s@CategoricalValues' {} a -> s {status = a} :: CategoricalValues)

instance Data.FromJSON CategoricalValues where
  parseJSON =
    Data.withObject
      "CategoricalValues"
      ( \x ->
          CategoricalValues'
            Prelude.<$> (x Data..:? "NumberOfCategory")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable CategoricalValues where
  hashWithSalt _salt CategoricalValues' {..} =
    _salt `Prelude.hashWithSalt` numberOfCategory
      `Prelude.hashWithSalt` status

instance Prelude.NFData CategoricalValues where
  rnf CategoricalValues' {..} =
    Prelude.rnf numberOfCategory
      `Prelude.seq` Prelude.rnf status

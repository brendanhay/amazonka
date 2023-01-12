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
-- Module      : Amazonka.LakeFormation.Types.PartitionObjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.PartitionObjects where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.TableObject
import qualified Amazonka.Prelude as Prelude

-- | A structure containing a list of partition values and table objects.
--
-- /See:/ 'newPartitionObjects' smart constructor.
data PartitionObjects = PartitionObjects'
  { -- | A list of table objects
    objects :: Prelude.Maybe [TableObject],
    -- | A list of partition values.
    partitionValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objects', 'partitionObjects_objects' - A list of table objects
--
-- 'partitionValues', 'partitionObjects_partitionValues' - A list of partition values.
newPartitionObjects ::
  PartitionObjects
newPartitionObjects =
  PartitionObjects'
    { objects = Prelude.Nothing,
      partitionValues = Prelude.Nothing
    }

-- | A list of table objects
partitionObjects_objects :: Lens.Lens' PartitionObjects (Prelude.Maybe [TableObject])
partitionObjects_objects = Lens.lens (\PartitionObjects' {objects} -> objects) (\s@PartitionObjects' {} a -> s {objects = a} :: PartitionObjects) Prelude.. Lens.mapping Lens.coerced

-- | A list of partition values.
partitionObjects_partitionValues :: Lens.Lens' PartitionObjects (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
partitionObjects_partitionValues = Lens.lens (\PartitionObjects' {partitionValues} -> partitionValues) (\s@PartitionObjects' {} a -> s {partitionValues = a} :: PartitionObjects) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PartitionObjects where
  parseJSON =
    Data.withObject
      "PartitionObjects"
      ( \x ->
          PartitionObjects'
            Prelude.<$> (x Data..:? "Objects" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PartitionValues")
      )

instance Prelude.Hashable PartitionObjects where
  hashWithSalt _salt PartitionObjects' {..} =
    _salt `Prelude.hashWithSalt` objects
      `Prelude.hashWithSalt` partitionValues

instance Prelude.NFData PartitionObjects where
  rnf PartitionObjects' {..} =
    Prelude.rnf objects
      `Prelude.seq` Prelude.rnf partitionValues

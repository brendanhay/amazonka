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
-- Module      : Amazonka.Glue.Types.UnfilteredPartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UnfilteredPartition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Partition
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUnfilteredPartition' smart constructor.
data UnfilteredPartition = UnfilteredPartition'
  { authorizedColumns :: Prelude.Maybe [Prelude.Text],
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    partition :: Prelude.Maybe Partition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnfilteredPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedColumns', 'unfilteredPartition_authorizedColumns' - Undocumented member.
--
-- 'isRegisteredWithLakeFormation', 'unfilteredPartition_isRegisteredWithLakeFormation' - Undocumented member.
--
-- 'partition', 'unfilteredPartition_partition' - Undocumented member.
newUnfilteredPartition ::
  UnfilteredPartition
newUnfilteredPartition =
  UnfilteredPartition'
    { authorizedColumns =
        Prelude.Nothing,
      isRegisteredWithLakeFormation = Prelude.Nothing,
      partition = Prelude.Nothing
    }

-- | Undocumented member.
unfilteredPartition_authorizedColumns :: Lens.Lens' UnfilteredPartition (Prelude.Maybe [Prelude.Text])
unfilteredPartition_authorizedColumns = Lens.lens (\UnfilteredPartition' {authorizedColumns} -> authorizedColumns) (\s@UnfilteredPartition' {} a -> s {authorizedColumns = a} :: UnfilteredPartition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
unfilteredPartition_isRegisteredWithLakeFormation :: Lens.Lens' UnfilteredPartition (Prelude.Maybe Prelude.Bool)
unfilteredPartition_isRegisteredWithLakeFormation = Lens.lens (\UnfilteredPartition' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@UnfilteredPartition' {} a -> s {isRegisteredWithLakeFormation = a} :: UnfilteredPartition)

-- | Undocumented member.
unfilteredPartition_partition :: Lens.Lens' UnfilteredPartition (Prelude.Maybe Partition)
unfilteredPartition_partition = Lens.lens (\UnfilteredPartition' {partition} -> partition) (\s@UnfilteredPartition' {} a -> s {partition = a} :: UnfilteredPartition)

instance Data.FromJSON UnfilteredPartition where
  parseJSON =
    Data.withObject
      "UnfilteredPartition"
      ( \x ->
          UnfilteredPartition'
            Prelude.<$> ( x Data..:? "AuthorizedColumns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Data..:? "Partition")
      )

instance Prelude.Hashable UnfilteredPartition where
  hashWithSalt _salt UnfilteredPartition' {..} =
    _salt `Prelude.hashWithSalt` authorizedColumns
      `Prelude.hashWithSalt` isRegisteredWithLakeFormation
      `Prelude.hashWithSalt` partition

instance Prelude.NFData UnfilteredPartition where
  rnf UnfilteredPartition' {..} =
    Prelude.rnf authorizedColumns
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf partition

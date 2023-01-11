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
-- Module      : Amazonka.KeySpaces.Types.PartitionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.PartitionKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The partition key portion of the primary key is required and determines
-- how Amazon Keyspaces stores the data. The partition key can be a single
-- column, or it can be a compound value composed of two or more columns.
--
-- /See:/ 'newPartitionKey' smart constructor.
data PartitionKey = PartitionKey'
  { -- | The name(s) of the partition key column(s).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'partitionKey_name' - The name(s) of the partition key column(s).
newPartitionKey ::
  -- | 'name'
  Prelude.Text ->
  PartitionKey
newPartitionKey pName_ = PartitionKey' {name = pName_}

-- | The name(s) of the partition key column(s).
partitionKey_name :: Lens.Lens' PartitionKey Prelude.Text
partitionKey_name = Lens.lens (\PartitionKey' {name} -> name) (\s@PartitionKey' {} a -> s {name = a} :: PartitionKey)

instance Data.FromJSON PartitionKey where
  parseJSON =
    Data.withObject
      "PartitionKey"
      (\x -> PartitionKey' Prelude.<$> (x Data..: "name"))

instance Prelude.Hashable PartitionKey where
  hashWithSalt _salt PartitionKey' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData PartitionKey where
  rnf PartitionKey' {..} = Prelude.rnf name

instance Data.ToJSON PartitionKey where
  toJSON PartitionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

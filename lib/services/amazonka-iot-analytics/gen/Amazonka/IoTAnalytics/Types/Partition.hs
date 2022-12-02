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
-- Module      : Amazonka.IoTAnalytics.Types.Partition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Partition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A partition dimension defined by an attribute.
--
-- /See:/ 'newPartition' smart constructor.
data Partition = Partition'
  { -- | The name of the attribute that defines a partition dimension.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Partition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'partition_attributeName' - The name of the attribute that defines a partition dimension.
newPartition ::
  -- | 'attributeName'
  Prelude.Text ->
  Partition
newPartition pAttributeName_ =
  Partition' {attributeName = pAttributeName_}

-- | The name of the attribute that defines a partition dimension.
partition_attributeName :: Lens.Lens' Partition Prelude.Text
partition_attributeName = Lens.lens (\Partition' {attributeName} -> attributeName) (\s@Partition' {} a -> s {attributeName = a} :: Partition)

instance Data.FromJSON Partition where
  parseJSON =
    Data.withObject
      "Partition"
      ( \x ->
          Partition' Prelude.<$> (x Data..: "attributeName")
      )

instance Prelude.Hashable Partition where
  hashWithSalt _salt Partition' {..} =
    _salt `Prelude.hashWithSalt` attributeName

instance Prelude.NFData Partition where
  rnf Partition' {..} = Prelude.rnf attributeName

instance Data.ToJSON Partition where
  toJSON Partition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("attributeName" Data..= attributeName)
          ]
      )

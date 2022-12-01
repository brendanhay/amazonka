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
-- Module      : Amazonka.Pi.Types.ResponsePartitionKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.ResponsePartitionKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | If @PartitionBy@ was specified in a @DescribeDimensionKeys@ request, the
-- dimensions are returned in an array. Each element in the array specifies
-- one dimension.
--
-- /See:/ 'newResponsePartitionKey' smart constructor.
data ResponsePartitionKey = ResponsePartitionKey'
  { -- | A dimension map that contains the dimensions for this partition.
    dimensions :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponsePartitionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'responsePartitionKey_dimensions' - A dimension map that contains the dimensions for this partition.
newResponsePartitionKey ::
  ResponsePartitionKey
newResponsePartitionKey =
  ResponsePartitionKey' {dimensions = Prelude.mempty}

-- | A dimension map that contains the dimensions for this partition.
responsePartitionKey_dimensions :: Lens.Lens' ResponsePartitionKey (Prelude.HashMap Prelude.Text Prelude.Text)
responsePartitionKey_dimensions = Lens.lens (\ResponsePartitionKey' {dimensions} -> dimensions) (\s@ResponsePartitionKey' {} a -> s {dimensions = a} :: ResponsePartitionKey) Prelude.. Lens.coerced

instance Core.FromJSON ResponsePartitionKey where
  parseJSON =
    Core.withObject
      "ResponsePartitionKey"
      ( \x ->
          ResponsePartitionKey'
            Prelude.<$> (x Core..:? "Dimensions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResponsePartitionKey where
  hashWithSalt _salt ResponsePartitionKey' {..} =
    _salt `Prelude.hashWithSalt` dimensions

instance Prelude.NFData ResponsePartitionKey where
  rnf ResponsePartitionKey' {..} =
    Prelude.rnf dimensions

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
-- Module      : Amazonka.Batch.Types.ArrayProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ArrayProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch array job.
--
-- /See:/ 'newArrayProperties' smart constructor.
data ArrayProperties = ArrayProperties'
  { -- | The size of the array job.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArrayProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'arrayProperties_size' - The size of the array job.
newArrayProperties ::
  ArrayProperties
newArrayProperties =
  ArrayProperties' {size = Prelude.Nothing}

-- | The size of the array job.
arrayProperties_size :: Lens.Lens' ArrayProperties (Prelude.Maybe Prelude.Int)
arrayProperties_size = Lens.lens (\ArrayProperties' {size} -> size) (\s@ArrayProperties' {} a -> s {size = a} :: ArrayProperties)

instance Prelude.Hashable ArrayProperties where
  hashWithSalt _salt ArrayProperties' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData ArrayProperties where
  rnf ArrayProperties' {..} = Prelude.rnf size

instance Core.ToJSON ArrayProperties where
  toJSON ArrayProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [("size" Core..=) Prelude.<$> size]
      )

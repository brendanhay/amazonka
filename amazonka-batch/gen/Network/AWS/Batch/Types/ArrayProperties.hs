{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.ArrayProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an AWS Batch array job.
--
-- /See:/ 'newArrayProperties' smart constructor.
data ArrayProperties = ArrayProperties'
  { -- | The size of the array job.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable ArrayProperties

instance Prelude.NFData ArrayProperties

instance Prelude.ToJSON ArrayProperties where
  toJSON ArrayProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("size" Prelude..=) Prelude.<$> size]
      )

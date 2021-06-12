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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an AWS Batch array job.
--
-- /See:/ 'newArrayProperties' smart constructor.
data ArrayProperties = ArrayProperties'
  { -- | The size of the array job.
    size :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ArrayProperties' {size = Core.Nothing}

-- | The size of the array job.
arrayProperties_size :: Lens.Lens' ArrayProperties (Core.Maybe Core.Int)
arrayProperties_size = Lens.lens (\ArrayProperties' {size} -> size) (\s@ArrayProperties' {} a -> s {size = a} :: ArrayProperties)

instance Core.Hashable ArrayProperties

instance Core.NFData ArrayProperties

instance Core.ToJSON ArrayProperties where
  toJSON ArrayProperties' {..} =
    Core.object
      (Core.catMaybes [("size" Core..=) Core.<$> size])

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
-- Module      : Network.AWS.AppStream.Types.ComputeCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the capacity for a fleet.
--
-- /See:/ 'newComputeCapacity' smart constructor.
data ComputeCapacity = ComputeCapacity'
  { -- | The desired number of streaming instances.
    desiredInstances :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComputeCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredInstances', 'computeCapacity_desiredInstances' - The desired number of streaming instances.
newComputeCapacity ::
  -- | 'desiredInstances'
  Core.Int ->
  ComputeCapacity
newComputeCapacity pDesiredInstances_ =
  ComputeCapacity'
    { desiredInstances =
        pDesiredInstances_
    }

-- | The desired number of streaming instances.
computeCapacity_desiredInstances :: Lens.Lens' ComputeCapacity Core.Int
computeCapacity_desiredInstances = Lens.lens (\ComputeCapacity' {desiredInstances} -> desiredInstances) (\s@ComputeCapacity' {} a -> s {desiredInstances = a} :: ComputeCapacity)

instance Core.Hashable ComputeCapacity

instance Core.NFData ComputeCapacity

instance Core.ToJSON ComputeCapacity where
  toJSON ComputeCapacity' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DesiredInstances" Core..= desiredInstances)
          ]
      )

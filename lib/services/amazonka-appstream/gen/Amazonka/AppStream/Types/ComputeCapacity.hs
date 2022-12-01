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
-- Module      : Amazonka.AppStream.Types.ComputeCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ComputeCapacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the capacity for a fleet.
--
-- /See:/ 'newComputeCapacity' smart constructor.
data ComputeCapacity = ComputeCapacity'
  { -- | The desired number of streaming instances.
    desiredInstances :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ComputeCapacity
newComputeCapacity pDesiredInstances_ =
  ComputeCapacity'
    { desiredInstances =
        pDesiredInstances_
    }

-- | The desired number of streaming instances.
computeCapacity_desiredInstances :: Lens.Lens' ComputeCapacity Prelude.Int
computeCapacity_desiredInstances = Lens.lens (\ComputeCapacity' {desiredInstances} -> desiredInstances) (\s@ComputeCapacity' {} a -> s {desiredInstances = a} :: ComputeCapacity)

instance Prelude.Hashable ComputeCapacity where
  hashWithSalt _salt ComputeCapacity' {..} =
    _salt `Prelude.hashWithSalt` desiredInstances

instance Prelude.NFData ComputeCapacity where
  rnf ComputeCapacity' {..} =
    Prelude.rnf desiredInstances

instance Core.ToJSON ComputeCapacity where
  toJSON ComputeCapacity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DesiredInstances" Core..= desiredInstances)
          ]
      )

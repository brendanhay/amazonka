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
-- Module      : Amazonka.WorkSpaces.Types.ComputeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ComputeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.Compute

-- | Describes the compute type of the bundle.
--
-- /See:/ 'newComputeType' smart constructor.
data ComputeType = ComputeType'
  { -- | The compute type.
    name :: Prelude.Maybe Compute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'computeType_name' - The compute type.
newComputeType ::
  ComputeType
newComputeType = ComputeType' {name = Prelude.Nothing}

-- | The compute type.
computeType_name :: Lens.Lens' ComputeType (Prelude.Maybe Compute)
computeType_name = Lens.lens (\ComputeType' {name} -> name) (\s@ComputeType' {} a -> s {name = a} :: ComputeType)

instance Core.FromJSON ComputeType where
  parseJSON =
    Core.withObject
      "ComputeType"
      (\x -> ComputeType' Prelude.<$> (x Core..:? "Name"))

instance Prelude.Hashable ComputeType where
  hashWithSalt _salt ComputeType' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ComputeType where
  rnf ComputeType' {..} = Prelude.rnf name

instance Core.ToJSON ComputeType where
  toJSON ComputeType' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Name" Core..=) Prelude.<$> name]
      )

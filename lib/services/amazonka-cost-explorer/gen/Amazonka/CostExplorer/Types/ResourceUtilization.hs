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
-- Module      : Amazonka.CostExplorer.Types.ResourceUtilization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.EC2ResourceUtilization
import qualified Amazonka.Prelude as Prelude

-- | Resource utilization of current resource.
--
-- /See:/ 'newResourceUtilization' smart constructor.
data ResourceUtilization = ResourceUtilization'
  { -- | The utilization of current Amazon EC2 instance.
    eC2ResourceUtilization :: Prelude.Maybe EC2ResourceUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2ResourceUtilization', 'resourceUtilization_eC2ResourceUtilization' - The utilization of current Amazon EC2 instance.
newResourceUtilization ::
  ResourceUtilization
newResourceUtilization =
  ResourceUtilization'
    { eC2ResourceUtilization =
        Prelude.Nothing
    }

-- | The utilization of current Amazon EC2 instance.
resourceUtilization_eC2ResourceUtilization :: Lens.Lens' ResourceUtilization (Prelude.Maybe EC2ResourceUtilization)
resourceUtilization_eC2ResourceUtilization = Lens.lens (\ResourceUtilization' {eC2ResourceUtilization} -> eC2ResourceUtilization) (\s@ResourceUtilization' {} a -> s {eC2ResourceUtilization = a} :: ResourceUtilization)

instance Core.FromJSON ResourceUtilization where
  parseJSON =
    Core.withObject
      "ResourceUtilization"
      ( \x ->
          ResourceUtilization'
            Prelude.<$> (x Core..:? "EC2ResourceUtilization")
      )

instance Prelude.Hashable ResourceUtilization where
  hashWithSalt _salt ResourceUtilization' {..} =
    _salt `Prelude.hashWithSalt` eC2ResourceUtilization

instance Prelude.NFData ResourceUtilization where
  rnf ResourceUtilization' {..} =
    Prelude.rnf eC2ResourceUtilization

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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.TargetResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.TargetResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.NLBResource
import Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord

-- | The target resource that the Route 53 record points to.
--
-- /See:/ 'newTargetResource' smart constructor.
data TargetResource = TargetResource'
  { -- | The Network Load Balancer Resource.
    nLBResource :: Prelude.Maybe NLBResource,
    -- | The Route 53 resource.
    r53Resource :: Prelude.Maybe R53ResourceRecord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nLBResource', 'targetResource_nLBResource' - The Network Load Balancer Resource.
--
-- 'r53Resource', 'targetResource_r53Resource' - The Route 53 resource.
newTargetResource ::
  TargetResource
newTargetResource =
  TargetResource'
    { nLBResource = Prelude.Nothing,
      r53Resource = Prelude.Nothing
    }

-- | The Network Load Balancer Resource.
targetResource_nLBResource :: Lens.Lens' TargetResource (Prelude.Maybe NLBResource)
targetResource_nLBResource = Lens.lens (\TargetResource' {nLBResource} -> nLBResource) (\s@TargetResource' {} a -> s {nLBResource = a} :: TargetResource)

-- | The Route 53 resource.
targetResource_r53Resource :: Lens.Lens' TargetResource (Prelude.Maybe R53ResourceRecord)
targetResource_r53Resource = Lens.lens (\TargetResource' {r53Resource} -> r53Resource) (\s@TargetResource' {} a -> s {r53Resource = a} :: TargetResource)

instance Core.FromJSON TargetResource where
  parseJSON =
    Core.withObject
      "TargetResource"
      ( \x ->
          TargetResource'
            Prelude.<$> (x Core..:? "nLBResource")
            Prelude.<*> (x Core..:? "r53Resource")
      )

instance Prelude.Hashable TargetResource where
  hashWithSalt _salt TargetResource' {..} =
    _salt `Prelude.hashWithSalt` nLBResource
      `Prelude.hashWithSalt` r53Resource

instance Prelude.NFData TargetResource where
  rnf TargetResource' {..} =
    Prelude.rnf nLBResource
      `Prelude.seq` Prelude.rnf r53Resource

instance Core.ToJSON TargetResource where
  toJSON TargetResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nLBResource" Core..=) Prelude.<$> nLBResource,
            ("r53Resource" Core..=) Prelude.<$> r53Resource
          ]
      )

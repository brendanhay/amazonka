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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.TargetResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.NLBResource
import Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord

-- | The target resource the R53 record points to
--
-- /See:/ 'newTargetResource' smart constructor.
data TargetResource = TargetResource'
  { r53Resource :: Prelude.Maybe R53ResourceRecord,
    nLBResource :: Prelude.Maybe NLBResource
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
-- 'r53Resource', 'targetResource_r53Resource' - Undocumented member.
--
-- 'nLBResource', 'targetResource_nLBResource' - Undocumented member.
newTargetResource ::
  TargetResource
newTargetResource =
  TargetResource'
    { r53Resource = Prelude.Nothing,
      nLBResource = Prelude.Nothing
    }

-- | Undocumented member.
targetResource_r53Resource :: Lens.Lens' TargetResource (Prelude.Maybe R53ResourceRecord)
targetResource_r53Resource = Lens.lens (\TargetResource' {r53Resource} -> r53Resource) (\s@TargetResource' {} a -> s {r53Resource = a} :: TargetResource)

-- | Undocumented member.
targetResource_nLBResource :: Lens.Lens' TargetResource (Prelude.Maybe NLBResource)
targetResource_nLBResource = Lens.lens (\TargetResource' {nLBResource} -> nLBResource) (\s@TargetResource' {} a -> s {nLBResource = a} :: TargetResource)

instance Core.FromJSON TargetResource where
  parseJSON =
    Core.withObject
      "TargetResource"
      ( \x ->
          TargetResource'
            Prelude.<$> (x Core..:? "r53Resource")
            Prelude.<*> (x Core..:? "nLBResource")
      )

instance Prelude.Hashable TargetResource where
  hashWithSalt _salt TargetResource' {..} =
    _salt `Prelude.hashWithSalt` r53Resource
      `Prelude.hashWithSalt` nLBResource

instance Prelude.NFData TargetResource where
  rnf TargetResource' {..} =
    Prelude.rnf r53Resource
      `Prelude.seq` Prelude.rnf nLBResource

instance Core.ToJSON TargetResource where
  toJSON TargetResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("r53Resource" Core..=) Prelude.<$> r53Resource,
            ("nLBResource" Core..=) Prelude.<$> nLBResource
          ]
      )

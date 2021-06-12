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
-- Module      : Network.AWS.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ServiceSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.EC2Specification
import qualified Network.AWS.Lens as Lens

-- | Hardware specifications for the service that you want recommendations
-- for.
--
-- /See:/ 'newServiceSpecification' smart constructor.
data ServiceSpecification = ServiceSpecification'
  { -- | The Amazon EC2 hardware specifications that you want AWS to provide
    -- recommendations for.
    eC2Specification :: Core.Maybe EC2Specification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2Specification', 'serviceSpecification_eC2Specification' - The Amazon EC2 hardware specifications that you want AWS to provide
-- recommendations for.
newServiceSpecification ::
  ServiceSpecification
newServiceSpecification =
  ServiceSpecification'
    { eC2Specification =
        Core.Nothing
    }

-- | The Amazon EC2 hardware specifications that you want AWS to provide
-- recommendations for.
serviceSpecification_eC2Specification :: Lens.Lens' ServiceSpecification (Core.Maybe EC2Specification)
serviceSpecification_eC2Specification = Lens.lens (\ServiceSpecification' {eC2Specification} -> eC2Specification) (\s@ServiceSpecification' {} a -> s {eC2Specification = a} :: ServiceSpecification)

instance Core.FromJSON ServiceSpecification where
  parseJSON =
    Core.withObject
      "ServiceSpecification"
      ( \x ->
          ServiceSpecification'
            Core.<$> (x Core..:? "EC2Specification")
      )

instance Core.Hashable ServiceSpecification

instance Core.NFData ServiceSpecification

instance Core.ToJSON ServiceSpecification where
  toJSON ServiceSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EC2Specification" Core..=)
              Core.<$> eC2Specification
          ]
      )

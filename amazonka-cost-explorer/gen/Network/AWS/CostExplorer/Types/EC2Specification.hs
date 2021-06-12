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
-- Module      : Network.AWS.CostExplorer.Types.EC2Specification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2Specification where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.OfferingClass
import qualified Network.AWS.Lens as Lens

-- | The Amazon EC2 hardware specifications that you want AWS to provide
-- recommendations for.
--
-- /See:/ 'newEC2Specification' smart constructor.
data EC2Specification = EC2Specification'
  { -- | Whether you want a recommendation for standard or convertible
    -- reservations.
    offeringClass :: Core.Maybe OfferingClass
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EC2Specification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringClass', 'eC2Specification_offeringClass' - Whether you want a recommendation for standard or convertible
-- reservations.
newEC2Specification ::
  EC2Specification
newEC2Specification =
  EC2Specification' {offeringClass = Core.Nothing}

-- | Whether you want a recommendation for standard or convertible
-- reservations.
eC2Specification_offeringClass :: Lens.Lens' EC2Specification (Core.Maybe OfferingClass)
eC2Specification_offeringClass = Lens.lens (\EC2Specification' {offeringClass} -> offeringClass) (\s@EC2Specification' {} a -> s {offeringClass = a} :: EC2Specification)

instance Core.FromJSON EC2Specification where
  parseJSON =
    Core.withObject
      "EC2Specification"
      ( \x ->
          EC2Specification'
            Core.<$> (x Core..:? "OfferingClass")
      )

instance Core.Hashable EC2Specification

instance Core.NFData EC2Specification

instance Core.ToJSON EC2Specification where
  toJSON EC2Specification' {..} =
    Core.object
      ( Core.catMaybes
          [("OfferingClass" Core..=) Core.<$> offeringClass]
      )

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
-- Module      : Network.AWS.MediaConnect.Types.ResourceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types.ResourceSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConnect.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | A definition of what is being billed for, including the type and amount.
--
-- /See:/ 'newResourceSpecification' smart constructor.
data ResourceSpecification = ResourceSpecification'
  { -- | The amount of outbound bandwidth that is discounted in the offering.
    reservedBitrate :: Prelude.Maybe Prelude.Int,
    -- | The type of resource and the unit that is being billed for.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedBitrate', 'resourceSpecification_reservedBitrate' - The amount of outbound bandwidth that is discounted in the offering.
--
-- 'resourceType', 'resourceSpecification_resourceType' - The type of resource and the unit that is being billed for.
newResourceSpecification ::
  -- | 'resourceType'
  ResourceType ->
  ResourceSpecification
newResourceSpecification pResourceType_ =
  ResourceSpecification'
    { reservedBitrate =
        Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | The amount of outbound bandwidth that is discounted in the offering.
resourceSpecification_reservedBitrate :: Lens.Lens' ResourceSpecification (Prelude.Maybe Prelude.Int)
resourceSpecification_reservedBitrate = Lens.lens (\ResourceSpecification' {reservedBitrate} -> reservedBitrate) (\s@ResourceSpecification' {} a -> s {reservedBitrate = a} :: ResourceSpecification)

-- | The type of resource and the unit that is being billed for.
resourceSpecification_resourceType :: Lens.Lens' ResourceSpecification ResourceType
resourceSpecification_resourceType = Lens.lens (\ResourceSpecification' {resourceType} -> resourceType) (\s@ResourceSpecification' {} a -> s {resourceType = a} :: ResourceSpecification)

instance Core.FromJSON ResourceSpecification where
  parseJSON =
    Core.withObject
      "ResourceSpecification"
      ( \x ->
          ResourceSpecification'
            Prelude.<$> (x Core..:? "reservedBitrate")
            Prelude.<*> (x Core..: "resourceType")
      )

instance Prelude.Hashable ResourceSpecification

instance Prelude.NFData ResourceSpecification

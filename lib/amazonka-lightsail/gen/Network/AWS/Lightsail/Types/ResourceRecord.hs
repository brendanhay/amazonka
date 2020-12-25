{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceRecord
  ( ResourceRecord (..),

    -- * Smart constructor
    mkResourceRecord,

    -- * Lenses
    rrName,
    rrType,
    rrValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Lightsail.Types.Type as Types
import qualified Network.AWS.Lightsail.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the domain name system (DNS) records to add to your domain's DNS to validate it for an Amazon Lightsail certificate.
--
-- /See:/ 'mkResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the record.
    name :: Core.Maybe Types.Name,
    -- | The DNS record type.
    type' :: Core.Maybe Types.Type,
    -- | The value for the DNS record.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceRecord' value with any optional fields omitted.
mkResourceRecord ::
  ResourceRecord
mkResourceRecord =
  ResourceRecord'
    { name = Core.Nothing,
      type' = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the record.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ResourceRecord (Core.Maybe Types.Name)
rrName = Lens.field @"name"
{-# DEPRECATED rrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The DNS record type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRecord (Core.Maybe Types.Type)
rrType = Lens.field @"type'"
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The value for the DNS record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord (Core.Maybe Types.Value)
rrValue = Lens.field @"value"
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceRecord where
  parseJSON =
    Core.withObject "ResourceRecord" Core.$
      \x ->
        ResourceRecord'
          Core.<$> (x Core..:? "name")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "value")

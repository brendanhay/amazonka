{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aKey,
    aValue,
  )
where

import qualified Network.AWS.ECR.Types.Key as Types
import qualified Network.AWS.ECR.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'ImageScanFinding' data type.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The attribute key.
    key :: Types.Key,
    -- | The value assigned to the attribute key.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute ::
  -- | 'key'
  Types.Key ->
  Attribute
mkAttribute key = Attribute' {key, value = Core.Nothing}

-- | The attribute key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKey :: Lens.Lens' Attribute Types.Key
aKey = Lens.field @"key"
{-# DEPRECATED aKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value assigned to the attribute key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Core.Maybe Types.Value)
aValue = Lens.field @"value"
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject "Attribute" Core.$
      \x ->
        Attribute' Core.<$> (x Core..: "key") Core.<*> (x Core..:? "value")

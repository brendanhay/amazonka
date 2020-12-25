{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.QueryStringKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringKeyValuePair
  ( QueryStringKeyValuePair (..),

    -- * Smart constructor
    mkQueryStringKeyValuePair,

    -- * Lenses
    qskvpKey,
    qskvpValue,
  )
where

import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a key/value pair.
--
-- /See:/ 'mkQueryStringKeyValuePair' smart constructor.
data QueryStringKeyValuePair = QueryStringKeyValuePair'
  { -- | The key. You can omit the key.
    key :: Core.Maybe Types.StringValue,
    -- | The value.
    value :: Core.Maybe Types.StringValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStringKeyValuePair' value with any optional fields omitted.
mkQueryStringKeyValuePair ::
  QueryStringKeyValuePair
mkQueryStringKeyValuePair =
  QueryStringKeyValuePair'
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The key. You can omit the key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qskvpKey :: Lens.Lens' QueryStringKeyValuePair (Core.Maybe Types.StringValue)
qskvpKey = Lens.field @"key"
{-# DEPRECATED qskvpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qskvpValue :: Lens.Lens' QueryStringKeyValuePair (Core.Maybe Types.StringValue)
qskvpValue = Lens.field @"value"
{-# DEPRECATED qskvpValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML QueryStringKeyValuePair where
  parseXML x =
    QueryStringKeyValuePair'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")

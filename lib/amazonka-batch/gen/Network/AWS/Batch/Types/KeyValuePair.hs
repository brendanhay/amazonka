{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.KeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.KeyValuePair
  ( KeyValuePair (..),

    -- * Smart constructor
    mkKeyValuePair,

    -- * Lenses
    kvpName,
    kvpValue,
  )
where

import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key-value pair object.
--
-- /See:/ 'mkKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | The name of the key-value pair. For environment variables, this is the name of the environment variable.
    name :: Core.Maybe Types.String,
    -- | The value of the key-value pair. For environment variables, this is the value of the environment variable.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyValuePair' value with any optional fields omitted.
mkKeyValuePair ::
  KeyValuePair
mkKeyValuePair =
  KeyValuePair' {name = Core.Nothing, value = Core.Nothing}

-- | The name of the key-value pair. For environment variables, this is the name of the environment variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvpName :: Lens.Lens' KeyValuePair (Core.Maybe Types.String)
kvpName = Lens.field @"name"
{-# DEPRECATED kvpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the key-value pair. For environment variables, this is the value of the environment variable.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvpValue :: Lens.Lens' KeyValuePair (Core.Maybe Types.String)
kvpValue = Lens.field @"value"
{-# DEPRECATED kvpValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON KeyValuePair where
  toJSON KeyValuePair {..} =
    Core.object
      ( Core.catMaybes
          [("name" Core..=) Core.<$> name, ("value" Core..=) Core.<$> value]
      )

instance Core.FromJSON KeyValuePair where
  parseJSON =
    Core.withObject "KeyValuePair" Core.$
      \x ->
        KeyValuePair'
          Core.<$> (x Core..:? "name") Core.<*> (x Core..:? "value")

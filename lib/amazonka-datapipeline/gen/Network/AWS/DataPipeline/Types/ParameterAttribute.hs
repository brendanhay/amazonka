{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterAttribute
  ( ParameterAttribute (..),

    -- * Smart constructor
    mkParameterAttribute,

    -- * Lenses
    paKey,
    paStringValue,
  )
where

import qualified Network.AWS.DataPipeline.Types.AttributeValueString as Types
import qualified Network.AWS.DataPipeline.Types.Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'mkParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { -- | The field identifier.
    key :: Types.Key,
    -- | The field value, expressed as a String.
    stringValue :: Types.AttributeValueString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterAttribute' value with any optional fields omitted.
mkParameterAttribute ::
  -- | 'key'
  Types.Key ->
  -- | 'stringValue'
  Types.AttributeValueString ->
  ParameterAttribute
mkParameterAttribute key stringValue =
  ParameterAttribute' {key, stringValue}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paKey :: Lens.Lens' ParameterAttribute Types.Key
paKey = Lens.field @"key"
{-# DEPRECATED paKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paStringValue :: Lens.Lens' ParameterAttribute Types.AttributeValueString
paStringValue = Lens.field @"stringValue"
{-# DEPRECATED paStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

instance Core.FromJSON ParameterAttribute where
  toJSON ParameterAttribute {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("stringValue" Core..= stringValue)
          ]
      )

instance Core.FromJSON ParameterAttribute where
  parseJSON =
    Core.withObject "ParameterAttribute" Core.$
      \x ->
        ParameterAttribute'
          Core.<$> (x Core..: "key") Core.<*> (x Core..: "stringValue")

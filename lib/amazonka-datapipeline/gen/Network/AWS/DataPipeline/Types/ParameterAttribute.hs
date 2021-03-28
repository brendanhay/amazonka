{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.ParameterAttribute
  ( ParameterAttribute (..)
  -- * Smart constructor
  , mkParameterAttribute
  -- * Lenses
  , paKey
  , paStringValue
  ) where

import qualified Network.AWS.DataPipeline.Types.AttributeValueString as Types
import qualified Network.AWS.DataPipeline.Types.Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'mkParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { key :: Types.Key
    -- ^ The field identifier.
  , stringValue :: Types.AttributeValueString
    -- ^ The field value, expressed as a String.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterAttribute' value with any optional fields omitted.
mkParameterAttribute
    :: Types.Key -- ^ 'key'
    -> Types.AttributeValueString -- ^ 'stringValue'
    -> ParameterAttribute
mkParameterAttribute key stringValue
  = ParameterAttribute'{key, stringValue}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paKey :: Lens.Lens' ParameterAttribute Types.Key
paKey = Lens.field @"key"
{-# INLINEABLE paKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paStringValue :: Lens.Lens' ParameterAttribute Types.AttributeValueString
paStringValue = Lens.field @"stringValue"
{-# INLINEABLE paStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON ParameterAttribute where
        toJSON ParameterAttribute{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key),
                  Core.Just ("stringValue" Core..= stringValue)])

instance Core.FromJSON ParameterAttribute where
        parseJSON
          = Core.withObject "ParameterAttribute" Core.$
              \ x ->
                ParameterAttribute' Core.<$>
                  (x Core..: "key") Core.<*> x Core..: "stringValue"

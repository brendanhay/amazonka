{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
  ( StringAttributeConstraintsType (..)
  -- * Smart constructor
  , mkStringAttributeConstraintsType
  -- * Lenses
  , sactMaxLength
  , sactMinLength
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.MaxLength as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.MinLength as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The constraints associated with a string attribute.
--
-- /See:/ 'mkStringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { maxLength :: Core.Maybe Types.MaxLength
    -- ^ The maximum length.
  , minLength :: Core.Maybe Types.MinLength
    -- ^ The minimum length.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StringAttributeConstraintsType' value with any optional fields omitted.
mkStringAttributeConstraintsType
    :: StringAttributeConstraintsType
mkStringAttributeConstraintsType
  = StringAttributeConstraintsType'{maxLength = Core.Nothing,
                                    minLength = Core.Nothing}

-- | The maximum length.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sactMaxLength :: Lens.Lens' StringAttributeConstraintsType (Core.Maybe Types.MaxLength)
sactMaxLength = Lens.field @"maxLength"
{-# INLINEABLE sactMaxLength #-}
{-# DEPRECATED maxLength "Use generic-lens or generic-optics with 'maxLength' instead"  #-}

-- | The minimum length.
--
-- /Note:/ Consider using 'minLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sactMinLength :: Lens.Lens' StringAttributeConstraintsType (Core.Maybe Types.MinLength)
sactMinLength = Lens.field @"minLength"
{-# INLINEABLE sactMinLength #-}
{-# DEPRECATED minLength "Use generic-lens or generic-optics with 'minLength' instead"  #-}

instance Core.FromJSON StringAttributeConstraintsType where
        toJSON StringAttributeConstraintsType{..}
          = Core.object
              (Core.catMaybes
                 [("MaxLength" Core..=) Core.<$> maxLength,
                  ("MinLength" Core..=) Core.<$> minLength])

instance Core.FromJSON StringAttributeConstraintsType where
        parseJSON
          = Core.withObject "StringAttributeConstraintsType" Core.$
              \ x ->
                StringAttributeConstraintsType' Core.<$>
                  (x Core..:? "MaxLength") Core.<*> x Core..:? "MinLength"

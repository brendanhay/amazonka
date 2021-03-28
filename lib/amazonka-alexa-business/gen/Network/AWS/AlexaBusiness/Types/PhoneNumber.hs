{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.PhoneNumber
  ( PhoneNumber (..)
  -- * Smart constructor
  , mkPhoneNumber
  -- * Lenses
  , pnNumber
  , pnType
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Number as Types
import qualified Network.AWS.AlexaBusiness.Types.PhoneNumberType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The phone number for the contact containing the raw number and phone number type.
--
-- /See:/ 'mkPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { number :: Types.Number
    -- ^ The raw value of the phone number.
  , type' :: Types.PhoneNumberType
    -- ^ The type of the phone number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhoneNumber' value with any optional fields omitted.
mkPhoneNumber
    :: Types.Number -- ^ 'number'
    -> Types.PhoneNumberType -- ^ 'type\''
    -> PhoneNumber
mkPhoneNumber number type' = PhoneNumber'{number, type'}

-- | The raw value of the phone number.
--
-- /Note:/ Consider using 'number' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnNumber :: Lens.Lens' PhoneNumber Types.Number
pnNumber = Lens.field @"number"
{-# INLINEABLE pnNumber #-}
{-# DEPRECATED number "Use generic-lens or generic-optics with 'number' instead"  #-}

-- | The type of the phone number.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnType :: Lens.Lens' PhoneNumber Types.PhoneNumberType
pnType = Lens.field @"type'"
{-# INLINEABLE pnType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PhoneNumber where
        toJSON PhoneNumber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Number" Core..= number),
                  Core.Just ("Type" Core..= type')])

instance Core.FromJSON PhoneNumber where
        parseJSON
          = Core.withObject "PhoneNumber" Core.$
              \ x ->
                PhoneNumber' Core.<$>
                  (x Core..: "Number") Core.<*> x Core..: "Type"

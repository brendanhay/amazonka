{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Predicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.Predicate
  ( Predicate (..)
  -- * Smart constructor
  , mkPredicate
  -- * Lenses
  , pNegated
  , pType
  , pDataId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.PredicateType as Types
import qualified Network.AWS.WAF.Types.ResourceId as Types

-- | Specifies the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , and 'SizeConstraintSet' objects that you want to add to a @Rule@ and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44. 
--
-- /See:/ 'mkPredicate' smart constructor.
data Predicate = Predicate'
  { negated :: Core.Bool
    -- ^ Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address.
--
-- Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
  , type' :: Types.PredicateType
    -- ^ The type of predicate in a @Rule@ , such as @ByteMatch@ or @IPSet@ .
  , dataId :: Types.ResourceId
    -- ^ A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Predicate' value with any optional fields omitted.
mkPredicate
    :: Core.Bool -- ^ 'negated'
    -> Types.PredicateType -- ^ 'type\''
    -> Types.ResourceId -- ^ 'dataId'
    -> Predicate
mkPredicate negated type' dataId
  = Predicate'{negated, type', dataId}

-- | Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address.
--
-- Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
--
-- /Note:/ Consider using 'negated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pNegated :: Lens.Lens' Predicate Core.Bool
pNegated = Lens.field @"negated"
{-# INLINEABLE pNegated #-}
{-# DEPRECATED negated "Use generic-lens or generic-optics with 'negated' instead"  #-}

-- | The type of predicate in a @Rule@ , such as @ByteMatch@ or @IPSet@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Predicate Types.PredicateType
pType = Lens.field @"type'"
{-# INLINEABLE pType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
--
-- /Note:/ Consider using 'dataId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataId :: Lens.Lens' Predicate Types.ResourceId
pDataId = Lens.field @"dataId"
{-# INLINEABLE pDataId #-}
{-# DEPRECATED dataId "Use generic-lens or generic-optics with 'dataId' instead"  #-}

instance Core.FromJSON Predicate where
        toJSON Predicate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Negated" Core..= negated),
                  Core.Just ("Type" Core..= type'),
                  Core.Just ("DataId" Core..= dataId)])

instance Core.FromJSON Predicate where
        parseJSON
          = Core.withObject "Predicate" Core.$
              \ x ->
                Predicate' Core.<$>
                  (x Core..: "Negated") Core.<*> x Core..: "Type" Core.<*>
                    x Core..: "DataId"

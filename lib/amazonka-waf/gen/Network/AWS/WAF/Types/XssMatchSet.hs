{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XssMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.XssMatchSet
  ( XssMatchSet (..)
  -- * Smart constructor
  , mkXssMatchSet
  -- * Lenses
  , xmsXssMatchSetId
  , xmsXssMatchTuples
  , xmsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types
import qualified Network.AWS.WAF.Types.XssMatchTuple as Types

-- | A complex type that contains @XssMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a @XssMatchSet@ contains more than one @XssMatchTuple@ object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.
--
-- /See:/ 'mkXssMatchSet' smart constructor.
data XssMatchSet = XssMatchSet'
  { xssMatchSetId :: Types.ResourceId
    -- ^ A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
  , xssMatchTuples :: [Types.XssMatchTuple]
    -- ^ Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name, if any, of the @XssMatchSet@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'XssMatchSet' value with any optional fields omitted.
mkXssMatchSet
    :: Types.ResourceId -- ^ 'xssMatchSetId'
    -> XssMatchSet
mkXssMatchSet xssMatchSetId
  = XssMatchSet'{xssMatchSetId, xssMatchTuples = Core.mempty,
                 name = Core.Nothing}

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsXssMatchSetId :: Lens.Lens' XssMatchSet Types.ResourceId
xmsXssMatchSetId = Lens.field @"xssMatchSetId"
{-# INLINEABLE xmsXssMatchSetId #-}
{-# DEPRECATED xssMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead"  #-}

-- | Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
--
-- /Note:/ Consider using 'xssMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsXssMatchTuples :: Lens.Lens' XssMatchSet [Types.XssMatchTuple]
xmsXssMatchTuples = Lens.field @"xssMatchTuples"
{-# INLINEABLE xmsXssMatchTuples #-}
{-# DEPRECATED xssMatchTuples "Use generic-lens or generic-optics with 'xssMatchTuples' instead"  #-}

-- | The name, if any, of the @XssMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsName :: Lens.Lens' XssMatchSet (Core.Maybe Types.ResourceName)
xmsName = Lens.field @"name"
{-# INLINEABLE xmsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON XssMatchSet where
        parseJSON
          = Core.withObject "XssMatchSet" Core.$
              \ x ->
                XssMatchSet' Core.<$>
                  (x Core..: "XssMatchSetId") Core.<*>
                    x Core..:? "XssMatchTuples" Core..!= Core.mempty
                    Core.<*> x Core..:? "Name"

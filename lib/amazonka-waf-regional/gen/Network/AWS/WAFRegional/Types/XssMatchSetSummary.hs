{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.XssMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.XssMatchSetSummary
  ( XssMatchSetSummary (..)
  -- * Smart constructor
  , mkXssMatchSetSummary
  -- * Lenses
  , xmssXssMatchSetId
  , xmssName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | The @Id@ and @Name@ of an @XssMatchSet@ .
--
-- /See:/ 'mkXssMatchSetSummary' smart constructor.
data XssMatchSetSummary = XssMatchSetSummary'
  { xssMatchSetId :: Types.ResourceId
    -- ^ A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
  , name :: Types.ResourceName
    -- ^ The name of the @XssMatchSet@ , if any, specified by @Id@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'XssMatchSetSummary' value with any optional fields omitted.
mkXssMatchSetSummary
    :: Types.ResourceId -- ^ 'xssMatchSetId'
    -> Types.ResourceName -- ^ 'name'
    -> XssMatchSetSummary
mkXssMatchSetSummary xssMatchSetId name
  = XssMatchSetSummary'{xssMatchSetId, name}

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmssXssMatchSetId :: Lens.Lens' XssMatchSetSummary Types.ResourceId
xmssXssMatchSetId = Lens.field @"xssMatchSetId"
{-# INLINEABLE xmssXssMatchSetId #-}
{-# DEPRECATED xssMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead"  #-}

-- | The name of the @XssMatchSet@ , if any, specified by @Id@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmssName :: Lens.Lens' XssMatchSetSummary Types.ResourceName
xmssName = Lens.field @"name"
{-# INLINEABLE xmssName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON XssMatchSetSummary where
        parseJSON
          = Core.withObject "XssMatchSetSummary" Core.$
              \ x ->
                XssMatchSetSummary' Core.<$>
                  (x Core..: "XssMatchSetId") Core.<*> x Core..: "Name"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.RegexMatchSetSummary
  ( RegexMatchSetSummary (..)
  -- * Smart constructor
  , mkRegexMatchSetSummary
  -- * Lenses
  , rmssRegexMatchSetId
  , rmssName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

-- | Returned by 'ListRegexMatchSets' . Each @RegexMatchSetSummary@ object includes the @Name@ and @RegexMatchSetId@ for one 'RegexMatchSet' .
--
-- /See:/ 'mkRegexMatchSetSummary' smart constructor.
data RegexMatchSetSummary = RegexMatchSetSummary'
  { regexMatchSetId :: Types.ResourceId
    -- ^ The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
  , name :: Types.ResourceName
    -- ^ A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexMatchSetSummary' value with any optional fields omitted.
mkRegexMatchSetSummary
    :: Types.ResourceId -- ^ 'regexMatchSetId'
    -> Types.ResourceName -- ^ 'name'
    -> RegexMatchSetSummary
mkRegexMatchSetSummary regexMatchSetId name
  = RegexMatchSetSummary'{regexMatchSetId, name}

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmssRegexMatchSetId :: Lens.Lens' RegexMatchSetSummary Types.ResourceId
rmssRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# INLINEABLE rmssRegexMatchSetId #-}
{-# DEPRECATED regexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead"  #-}

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmssName :: Lens.Lens' RegexMatchSetSummary Types.ResourceName
rmssName = Lens.field @"name"
{-# INLINEABLE rmssName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RegexMatchSetSummary where
        parseJSON
          = Core.withObject "RegexMatchSetSummary" Core.$
              \ x ->
                RegexMatchSetSummary' Core.<$>
                  (x Core..: "RegexMatchSetId") Core.<*> x Core..: "Name"

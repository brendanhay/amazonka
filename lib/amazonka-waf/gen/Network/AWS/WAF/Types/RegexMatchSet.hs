{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.RegexMatchSet
  ( RegexMatchSet (..)
  -- * Smart constructor
  , mkRegexMatchSet
  -- * Lenses
  , rmsName
  , rmsRegexMatchSetId
  , rmsRegexMatchTuples
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.Name as Types
import qualified Network.AWS.WAF.Types.RegexMatchSetId as Types
import qualified Network.AWS.WAF.Types.RegexMatchTuple as Types

-- | In a 'GetRegexMatchSet' request, @RegexMatchSet@ is a complex type that contains the @RegexMatchSetId@ and @Name@ of a @RegexMatchSet@ , and the values that you specified when you updated the @RegexMatchSet@ .
--
-- The values are contained in a @RegexMatchTuple@ object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @RegexMatchSet@ contains more than one @RegexMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
-- /See:/ 'mkRegexMatchSet' smart constructor.
data RegexMatchSet = RegexMatchSet'
  { name :: Core.Maybe Types.Name
    -- ^ A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
  , regexMatchSetId :: Core.Maybe Types.RegexMatchSetId
    -- ^ The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ).
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
  , regexMatchTuples :: Core.Maybe [Types.RegexMatchTuple]
    -- ^ Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains: 
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header. 
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexMatchSet' value with any optional fields omitted.
mkRegexMatchSet
    :: RegexMatchSet
mkRegexMatchSet
  = RegexMatchSet'{name = Core.Nothing,
                   regexMatchSetId = Core.Nothing, regexMatchTuples = Core.Nothing}

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsName :: Lens.Lens' RegexMatchSet (Core.Maybe Types.Name)
rmsName = Lens.field @"name"
{-# INLINEABLE rmsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ).
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsRegexMatchSetId :: Lens.Lens' RegexMatchSet (Core.Maybe Types.RegexMatchSetId)
rmsRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# INLINEABLE rmsRegexMatchSetId #-}
{-# DEPRECATED regexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead"  #-}

-- | Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains: 
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header. 
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
-- /Note:/ Consider using 'regexMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsRegexMatchTuples :: Lens.Lens' RegexMatchSet (Core.Maybe [Types.RegexMatchTuple])
rmsRegexMatchTuples = Lens.field @"regexMatchTuples"
{-# INLINEABLE rmsRegexMatchTuples #-}
{-# DEPRECATED regexMatchTuples "Use generic-lens or generic-optics with 'regexMatchTuples' instead"  #-}

instance Core.FromJSON RegexMatchSet where
        parseJSON
          = Core.withObject "RegexMatchSet" Core.$
              \ x ->
                RegexMatchSet' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "RegexMatchSetId" Core.<*>
                    x Core..:? "RegexMatchTuples"

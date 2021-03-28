{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.RegexPatternSet
  ( RegexPatternSet (..)
  -- * Smart constructor
  , mkRegexPatternSet
  -- * Lenses
  , rpsRegexPatternSetId
  , rpsRegexPatternStrings
  , rpsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.Name as Types
import qualified Network.AWS.WAF.Types.RegexPatternSetId as Types
import qualified Network.AWS.WAF.Types.RegexPatternString as Types

-- | The @RegexPatternSet@ specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- /See:/ 'mkRegexPatternSet' smart constructor.
data RegexPatternSet = RegexPatternSet'
  { regexPatternSetId :: Types.RegexPatternSetId
    -- ^ The identifier for the @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
  , regexPatternStrings :: [Types.RegexPatternString]
    -- ^ Specifies the regular expression (regex) patterns that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
  , name :: Core.Maybe Types.Name
    -- ^ A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexPatternSet' value with any optional fields omitted.
mkRegexPatternSet
    :: Types.RegexPatternSetId -- ^ 'regexPatternSetId'
    -> RegexPatternSet
mkRegexPatternSet regexPatternSetId
  = RegexPatternSet'{regexPatternSetId,
                     regexPatternStrings = Core.mempty, name = Core.Nothing}

-- | The identifier for the @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsRegexPatternSetId :: Lens.Lens' RegexPatternSet Types.RegexPatternSetId
rpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# INLINEABLE rpsRegexPatternSetId #-}
{-# DEPRECATED regexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead"  #-}

-- | Specifies the regular expression (regex) patterns that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
--
-- /Note:/ Consider using 'regexPatternStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsRegexPatternStrings :: Lens.Lens' RegexPatternSet [Types.RegexPatternString]
rpsRegexPatternStrings = Lens.field @"regexPatternStrings"
{-# INLINEABLE rpsRegexPatternStrings #-}
{-# DEPRECATED regexPatternStrings "Use generic-lens or generic-optics with 'regexPatternStrings' instead"  #-}

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsName :: Lens.Lens' RegexPatternSet (Core.Maybe Types.Name)
rpsName = Lens.field @"name"
{-# INLINEABLE rpsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RegexPatternSet where
        parseJSON
          = Core.withObject "RegexPatternSet" Core.$
              \ x ->
                RegexPatternSet' Core.<$>
                  (x Core..: "RegexPatternSetId") Core.<*>
                    x Core..:? "RegexPatternStrings" Core..!= Core.mempty
                    Core.<*> x Core..:? "Name"

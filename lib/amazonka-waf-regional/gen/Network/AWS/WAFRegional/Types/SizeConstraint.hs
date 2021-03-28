{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SizeConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.SizeConstraint
  ( SizeConstraint (..)
  -- * Smart constructor
  , mkSizeConstraint
  -- * Lenses
  , scFieldToMatch
  , scTextTransformation
  , scComparisonOperator
  , scSize
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ComparisonOperator as Types
import qualified Network.AWS.WAFRegional.Types.FieldToMatch as Types
import qualified Network.AWS.WAFRegional.Types.TextTransformation as Types

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- /See:/ 'mkSizeConstraint' smart constructor.
data SizeConstraint = SizeConstraint'
  { fieldToMatch :: Types.FieldToMatch
    -- ^ Specifies where in a web request to look for the size constraint.
  , textTransformation :: Types.TextTransformation
    -- ^ Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
-- Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection. 
-- __NONE__ 
-- Specify @NONE@ if you don't want to perform any text transformations.
-- __CMD_LINE__ 
-- When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
--
--     * Delete the following characters: \ " ' ^
--
--
--     * Delete spaces before the following characters: / (
--
--
--     * Replace the following characters with a space: , ;
--
--
--     * Replace multiple spaces with one space
--
--
--     * Convert uppercase letters (A-Z) to lowercase (a-z)
--
--
-- __COMPRESS_WHITE_SPACE__ 
-- Use this option to replace the following characters with a space character (decimal 32):
--
--     * \f, formfeed, decimal 12
--
--
--     * \t, tab, decimal 9
--
--
--     * \n, newline, decimal 10
--
--
--     * \r, carriage return, decimal 13
--
--
--     * \v, vertical tab, decimal 11
--
--
--     * non-breaking space, decimal 160
--
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
-- __HTML_ENTITY_DECODE__ 
-- Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
--     * Replaces @(ampersand)quot;@ with @"@ 
--
--
--     * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
--
--     * Replaces @(ampersand)lt;@ with a "less than" symbol
--
--
--     * Replaces @(ampersand)gt;@ with @>@ 
--
--
--     * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters
--
--
--     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters
--
--
-- __LOWERCASE__ 
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
-- __URL_DECODE__ 
-- Use this option to decode a URL-encoded value.
  , comparisonOperator :: Types.ComparisonOperator
    -- ^ The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@ 
-- __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@ 
-- __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@ 
-- __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@ 
-- __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@ 
-- __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@ 
  , size :: Core.Natural
    -- ^ The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
-- If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SizeConstraint' value with any optional fields omitted.
mkSizeConstraint
    :: Types.FieldToMatch -- ^ 'fieldToMatch'
    -> Types.TextTransformation -- ^ 'textTransformation'
    -> Types.ComparisonOperator -- ^ 'comparisonOperator'
    -> Core.Natural -- ^ 'size'
    -> SizeConstraint
mkSizeConstraint fieldToMatch textTransformation comparisonOperator
  size
  = SizeConstraint'{fieldToMatch, textTransformation,
                    comparisonOperator, size}

-- | Specifies where in a web request to look for the size constraint.
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFieldToMatch :: Lens.Lens' SizeConstraint Types.FieldToMatch
scFieldToMatch = Lens.field @"fieldToMatch"
{-# INLINEABLE scFieldToMatch #-}
{-# DEPRECATED fieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead"  #-}

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
-- Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection. 
-- __NONE__ 
-- Specify @NONE@ if you don't want to perform any text transformations.
-- __CMD_LINE__ 
-- When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
--
--     * Delete the following characters: \ " ' ^
--
--
--     * Delete spaces before the following characters: / (
--
--
--     * Replace the following characters with a space: , ;
--
--
--     * Replace multiple spaces with one space
--
--
--     * Convert uppercase letters (A-Z) to lowercase (a-z)
--
--
-- __COMPRESS_WHITE_SPACE__ 
-- Use this option to replace the following characters with a space character (decimal 32):
--
--     * \f, formfeed, decimal 12
--
--
--     * \t, tab, decimal 9
--
--
--     * \n, newline, decimal 10
--
--
--     * \r, carriage return, decimal 13
--
--
--     * \v, vertical tab, decimal 11
--
--
--     * non-breaking space, decimal 160
--
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
-- __HTML_ENTITY_DECODE__ 
-- Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
--     * Replaces @(ampersand)quot;@ with @"@ 
--
--
--     * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
--
--     * Replaces @(ampersand)lt;@ with a "less than" symbol
--
--
--     * Replaces @(ampersand)gt;@ with @>@ 
--
--
--     * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters
--
--
--     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters
--
--
-- __LOWERCASE__ 
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
-- __URL_DECODE__ 
-- Use this option to decode a URL-encoded value.
--
-- /Note:/ Consider using 'textTransformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTextTransformation :: Lens.Lens' SizeConstraint Types.TextTransformation
scTextTransformation = Lens.field @"textTransformation"
{-# INLINEABLE scTextTransformation #-}
{-# DEPRECATED textTransformation "Use generic-lens or generic-optics with 'textTransformation' instead"  #-}

-- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@ 
-- __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@ 
-- __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@ 
-- __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@ 
-- __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@ 
-- __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@ 
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scComparisonOperator :: Lens.Lens' SizeConstraint Types.ComparisonOperator
scComparisonOperator = Lens.field @"comparisonOperator"
{-# INLINEABLE scComparisonOperator #-}
{-# DEPRECATED comparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead"  #-}

-- | The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
-- If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSize :: Lens.Lens' SizeConstraint Core.Natural
scSize = Lens.field @"size"
{-# INLINEABLE scSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.FromJSON SizeConstraint where
        toJSON SizeConstraint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FieldToMatch" Core..= fieldToMatch),
                  Core.Just ("TextTransformation" Core..= textTransformation),
                  Core.Just ("ComparisonOperator" Core..= comparisonOperator),
                  Core.Just ("Size" Core..= size)])

instance Core.FromJSON SizeConstraint where
        parseJSON
          = Core.withObject "SizeConstraint" Core.$
              \ x ->
                SizeConstraint' Core.<$>
                  (x Core..: "FieldToMatch") Core.<*> x Core..: "TextTransformation"
                    Core.<*> x Core..: "ComparisonOperator"
                    Core.<*> x Core..: "Size"

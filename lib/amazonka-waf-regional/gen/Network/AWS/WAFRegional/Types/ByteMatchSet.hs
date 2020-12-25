{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ByteMatchSet
  ( ByteMatchSet (..),

    -- * Smart constructor
    mkByteMatchSet,

    -- * Lenses
    bmsByteMatchSetId,
    bmsByteMatchTuples,
    bmsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ByteMatchTuple as Types
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | In a 'GetByteMatchSet' request, @ByteMatchSet@ is a complex type that contains the @ByteMatchSetId@ and @Name@ of a @ByteMatchSet@ , and the values that you specified when you updated the @ByteMatchSet@ .
--
-- A complex type that contains @ByteMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @ByteMatchSet@ contains more than one @ByteMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
-- /See:/ 'mkByteMatchSet' smart constructor.
data ByteMatchSet = ByteMatchSet'
  { -- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ).
    --
    -- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
    byteMatchSetId :: Types.ResourceId,
    -- | Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
    byteMatchTuples :: [Types.ByteMatchTuple],
    -- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
    name :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ByteMatchSet' value with any optional fields omitted.
mkByteMatchSet ::
  -- | 'byteMatchSetId'
  Types.ResourceId ->
  ByteMatchSet
mkByteMatchSet byteMatchSetId =
  ByteMatchSet'
    { byteMatchSetId,
      byteMatchTuples = Core.mempty,
      name = Core.Nothing
    }

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ).
--
-- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsByteMatchSetId :: Lens.Lens' ByteMatchSet Types.ResourceId
bmsByteMatchSetId = Lens.field @"byteMatchSetId"
{-# DEPRECATED bmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
--
-- /Note:/ Consider using 'byteMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsByteMatchTuples :: Lens.Lens' ByteMatchSet [Types.ByteMatchTuple]
bmsByteMatchTuples = Lens.field @"byteMatchTuples"
{-# DEPRECATED bmsByteMatchTuples "Use generic-lens or generic-optics with 'byteMatchTuples' instead." #-}

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsName :: Lens.Lens' ByteMatchSet (Core.Maybe Types.ResourceName)
bmsName = Lens.field @"name"
{-# DEPRECATED bmsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ByteMatchSet where
  parseJSON =
    Core.withObject "ByteMatchSet" Core.$
      \x ->
        ByteMatchSet'
          Core.<$> (x Core..: "ByteMatchSetId")
          Core.<*> (x Core..:? "ByteMatchTuples" Core..!= Core.mempty)
          Core.<*> (x Core..:? "Name")

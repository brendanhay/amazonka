{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.ListGrantsResponse
  ( ListGrantsResponse (..)
  -- * Smart constructor
  , mkListGrantsResponse
  -- * Lenses
  , lgrGrants
  , lgrNextMarker
  , lgrTruncated
  ) where

import qualified Network.AWS.KMS.Types.GrantListEntry as Types
import qualified Network.AWS.KMS.Types.NextMarker as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkListGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { grants :: Core.Maybe [Types.GrantListEntry]
    -- ^ A list of grants.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
  , truncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListGrantsResponse' value with any optional fields omitted.
mkListGrantsResponse
    :: ListGrantsResponse
mkListGrantsResponse
  = ListGrantsResponse'{grants = Core.Nothing,
                        nextMarker = Core.Nothing, truncated = Core.Nothing}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrGrants :: Lens.Lens' ListGrantsResponse (Core.Maybe [Types.GrantListEntry])
lgrGrants = Lens.field @"grants"
{-# INLINEABLE lgrGrants #-}
{-# DEPRECATED grants "Use generic-lens or generic-optics with 'grants' instead"  #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrNextMarker :: Lens.Lens' ListGrantsResponse (Core.Maybe Types.NextMarker)
lgrNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lgrNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrTruncated :: Lens.Lens' ListGrantsResponse (Core.Maybe Core.Bool)
lgrTruncated = Lens.field @"truncated"
{-# INLINEABLE lgrTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

instance Core.FromJSON ListGrantsResponse where
        parseJSON
          = Core.withObject "ListGrantsResponse" Core.$
              \ x ->
                ListGrantsResponse' Core.<$>
                  (x Core..:? "Grants") Core.<*> x Core..:? "NextMarker" Core.<*>
                    x Core..:? "Truncated"

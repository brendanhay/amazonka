{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ApplicationsResponse
  ( ApplicationsResponse (..)
  -- * Smart constructor
  , mkApplicationsResponse
  -- * Lenses
  , aItem
  , aNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ApplicationResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all of your applications.
--
-- /See:/ 'mkApplicationsResponse' smart constructor.
data ApplicationsResponse = ApplicationsResponse'
  { item :: Core.Maybe [Types.ApplicationResponse]
    -- ^ An array of responses, one for each application that was returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationsResponse' value with any optional fields omitted.
mkApplicationsResponse
    :: ApplicationsResponse
mkApplicationsResponse
  = ApplicationsResponse'{item = Core.Nothing,
                          nextToken = Core.Nothing}

-- | An array of responses, one for each application that was returned.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aItem :: Lens.Lens' ApplicationsResponse (Core.Maybe [Types.ApplicationResponse])
aItem = Lens.field @"item"
{-# INLINEABLE aItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNextToken :: Lens.Lens' ApplicationsResponse (Core.Maybe Core.Text)
aNextToken = Lens.field @"nextToken"
{-# INLINEABLE aNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON ApplicationsResponse where
        parseJSON
          = Core.withObject "ApplicationsResponse" Core.$
              \ x ->
                ApplicationsResponse' Core.<$>
                  (x Core..:? "Item") Core.<*> x Core..:? "NextToken"

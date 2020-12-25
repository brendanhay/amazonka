{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneysResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneysResponse
  ( JourneysResponse (..),

    -- * Smart constructor
    mkJourneysResponse,

    -- * Lenses
    jrItem,
    jrNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.JourneyResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status, configuration, and other settings for all the journeys that are associated with an application.
--
-- /See:/ 'mkJourneysResponse' smart constructor.
data JourneysResponse = JourneysResponse'
  { -- | An array of responses, one for each journey that's associated with the application.
    item :: [Types.JourneyResponse],
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'JourneysResponse' value with any optional fields omitted.
mkJourneysResponse ::
  JourneysResponse
mkJourneysResponse =
  JourneysResponse' {item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each journey that's associated with the application.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrItem :: Lens.Lens' JourneysResponse [Types.JourneyResponse]
jrItem = Lens.field @"item"
{-# DEPRECATED jrItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrNextToken :: Lens.Lens' JourneysResponse (Core.Maybe Core.Text)
jrNextToken = Lens.field @"nextToken"
{-# DEPRECATED jrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON JourneysResponse where
  parseJSON =
    Core.withObject "JourneysResponse" Core.$
      \x ->
        JourneysResponse'
          Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
          Core.<*> (x Core..:? "NextToken")

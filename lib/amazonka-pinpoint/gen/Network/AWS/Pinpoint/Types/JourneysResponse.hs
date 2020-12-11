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
    jNextToken,
    jItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneyResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status, configuration, and other settings for all the journeys that are associated with an application.
--
-- /See:/ 'mkJourneysResponse' smart constructor.
data JourneysResponse = JourneysResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    item :: [JourneyResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneysResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each journey that's associated with the application.
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
mkJourneysResponse ::
  JourneysResponse
mkJourneysResponse =
  JourneysResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNextToken :: Lens.Lens' JourneysResponse (Lude.Maybe Lude.Text)
jNextToken = Lens.lens (nextToken :: JourneysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: JourneysResponse)
{-# DEPRECATED jNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each journey that's associated with the application.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jItem :: Lens.Lens' JourneysResponse [JourneyResponse]
jItem = Lens.lens (item :: JourneysResponse -> [JourneyResponse]) (\s a -> s {item = a} :: JourneysResponse)
{-# DEPRECATED jItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON JourneysResponse where
  parseJSON =
    Lude.withObject
      "JourneysResponse"
      ( \x ->
          JourneysResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )

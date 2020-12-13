{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ActivitiesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ActivitiesResponse
  ( ActivitiesResponse (..),

    -- * Smart constructor
    mkActivitiesResponse,

    -- * Lenses
    aNextToken,
    aItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ActivityResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the activities that were performed by a campaign.
--
-- /See:/ 'mkActivitiesResponse' smart constructor.
data ActivitiesResponse = ActivitiesResponse'
  { -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of responses, one for each activity that was performed by the campaign.
    item :: [ActivityResponse]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivitiesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
-- * 'item' - An array of responses, one for each activity that was performed by the campaign.
mkActivitiesResponse ::
  ActivitiesResponse
mkActivitiesResponse =
  ActivitiesResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNextToken :: Lens.Lens' ActivitiesResponse (Lude.Maybe Lude.Text)
aNextToken = Lens.lens (nextToken :: ActivitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ActivitiesResponse)
{-# DEPRECATED aNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each activity that was performed by the campaign.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aItem :: Lens.Lens' ActivitiesResponse [ActivityResponse]
aItem = Lens.lens (item :: ActivitiesResponse -> [ActivityResponse]) (\s a -> s {item = a} :: ActivitiesResponse)
{-# DEPRECATED aItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON ActivitiesResponse where
  parseJSON =
    Lude.withObject
      "ActivitiesResponse"
      ( \x ->
          ActivitiesResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )

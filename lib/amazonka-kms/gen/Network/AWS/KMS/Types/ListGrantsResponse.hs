{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ListGrantsResponse
  ( ListGrantsResponse (..),

    -- * Smart constructor
    mkListGrantsResponse,

    -- * Lenses
    lgTruncated,
    lgGrants,
    lgNextMarker,
  )
where

import Network.AWS.KMS.Types.GrantListEntry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkListGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { -- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
    truncated :: Lude.Maybe Lude.Bool,
    -- | A list of grants.
    grants :: Lude.Maybe [GrantListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGrantsResponse' with the minimum fields required to make a request.
--
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
-- * 'grants' - A list of grants.
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
mkListGrantsResponse ::
  ListGrantsResponse
mkListGrantsResponse =
  ListGrantsResponse'
    { truncated = Lude.Nothing,
      grants = Lude.Nothing,
      nextMarker = Lude.Nothing
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgTruncated :: Lens.Lens' ListGrantsResponse (Lude.Maybe Lude.Bool)
lgTruncated = Lens.lens (truncated :: ListGrantsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ListGrantsResponse)
{-# DEPRECATED lgTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgGrants :: Lens.Lens' ListGrantsResponse (Lude.Maybe [GrantListEntry])
lgGrants = Lens.lens (grants :: ListGrantsResponse -> Lude.Maybe [GrantListEntry]) (\s a -> s {grants = a} :: ListGrantsResponse)
{-# DEPRECATED lgGrants "Use generic-lens or generic-optics with 'grants' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextMarker :: Lens.Lens' ListGrantsResponse (Lude.Maybe Lude.Text)
lgNextMarker = Lens.lens (nextMarker :: ListGrantsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListGrantsResponse)
{-# DEPRECATED lgNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Lude.FromJSON ListGrantsResponse where
  parseJSON =
    Lude.withObject
      "ListGrantsResponse"
      ( \x ->
          ListGrantsResponse'
            Lude.<$> (x Lude..:? "Truncated")
            Lude.<*> (x Lude..:? "Grants" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextMarker")
      )

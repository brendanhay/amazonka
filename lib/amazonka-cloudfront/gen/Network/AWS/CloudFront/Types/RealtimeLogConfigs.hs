{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfigs
  ( RealtimeLogConfigs (..),

    -- * Smart constructor
    mkRealtimeLogConfigs,

    -- * Lenses
    rlcItems,
    rlcMarker,
    rlcMaxItems,
    rlcNextMarker,
    rlcIsTruncated,
  )
where

import Network.AWS.CloudFront.Types.RealtimeLogConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of real-time log configurations.
--
-- /See:/ 'mkRealtimeLogConfigs' smart constructor.
data RealtimeLogConfigs = RealtimeLogConfigs'
  { -- | Contains the list of real-time log configurations.
    items :: Lude.Maybe [RealtimeLogConfig],
    -- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
    marker :: Lude.Text,
    -- | The maximum number of real-time log configurations requested.
    maxItems :: Lude.Int,
    -- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
    isTruncated :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RealtimeLogConfigs' with the minimum fields required to make a request.
--
-- * 'items' - Contains the list of real-time log configurations.
-- * 'marker' - This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
-- * 'maxItems' - The maximum number of real-time log configurations requested.
-- * 'nextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
-- * 'isTruncated' - A flag that indicates whether there are more real-time log configurations than are contained in this list.
mkRealtimeLogConfigs ::
  -- | 'marker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  RealtimeLogConfigs
mkRealtimeLogConfigs pMarker_ pMaxItems_ pIsTruncated_ =
  RealtimeLogConfigs'
    { items = Lude.Nothing,
      marker = pMarker_,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing,
      isTruncated = pIsTruncated_
    }

-- | Contains the list of real-time log configurations.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcItems :: Lens.Lens' RealtimeLogConfigs (Lude.Maybe [RealtimeLogConfig])
rlcItems = Lens.lens (items :: RealtimeLogConfigs -> Lude.Maybe [RealtimeLogConfig]) (\s a -> s {items = a} :: RealtimeLogConfigs)
{-# DEPRECATED rlcItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMarker :: Lens.Lens' RealtimeLogConfigs Lude.Text
rlcMarker = Lens.lens (marker :: RealtimeLogConfigs -> Lude.Text) (\s a -> s {marker = a} :: RealtimeLogConfigs)
{-# DEPRECATED rlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of real-time log configurations requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMaxItems :: Lens.Lens' RealtimeLogConfigs Lude.Int
rlcMaxItems = Lens.lens (maxItems :: RealtimeLogConfigs -> Lude.Int) (\s a -> s {maxItems = a} :: RealtimeLogConfigs)
{-# DEPRECATED rlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcNextMarker :: Lens.Lens' RealtimeLogConfigs (Lude.Maybe Lude.Text)
rlcNextMarker = Lens.lens (nextMarker :: RealtimeLogConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: RealtimeLogConfigs)
{-# DEPRECATED rlcNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcIsTruncated :: Lens.Lens' RealtimeLogConfigs Lude.Bool
rlcIsTruncated = Lens.lens (isTruncated :: RealtimeLogConfigs -> Lude.Bool) (\s a -> s {isTruncated = a} :: RealtimeLogConfigs)
{-# DEPRECATED rlcIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

instance Lude.FromXML RealtimeLogConfigs where
  parseXML x =
    RealtimeLogConfigs'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@ "Marker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "IsTruncated")

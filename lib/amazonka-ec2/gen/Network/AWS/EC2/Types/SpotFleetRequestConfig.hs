{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfig
  ( SpotFleetRequestConfig (..),

    -- * Smart constructor
    mkSpotFleetRequestConfig,

    -- * Lenses
    sfrcSpotFleetRequestConfig,
    sfrcSpotFleetRequestId,
    sfrcSpotFleetRequestState,
    sfrcCreateTime,
    sfrcTags,
    sfrcActivityStatus,
  )
where

import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Fleet request.
--
-- /See:/ 'mkSpotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { spotFleetRequestConfig ::
      Lude.Maybe SpotFleetRequestConfigData,
    spotFleetRequestId :: Lude.Maybe Lude.Text,
    spotFleetRequestState ::
      Lude.Maybe BatchState,
    createTime :: Lude.Maybe Lude.DateTime,
    tags :: Lude.Maybe [Tag],
    activityStatus :: Lude.Maybe ActivityStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotFleetRequestConfig' with the minimum fields required to make a request.
--
-- * 'activityStatus' - The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
-- * 'createTime' - The creation date and time of the request.
-- * 'spotFleetRequestConfig' - The configuration of the Spot Fleet request.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
-- * 'spotFleetRequestState' - The state of the Spot Fleet request.
-- * 'tags' - The tags for a Spot Fleet resource.
mkSpotFleetRequestConfig ::
  SpotFleetRequestConfig
mkSpotFleetRequestConfig =
  SpotFleetRequestConfig'
    { spotFleetRequestConfig = Lude.Nothing,
      spotFleetRequestId = Lude.Nothing,
      spotFleetRequestState = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing,
      activityStatus = Lude.Nothing
    }

-- | The configuration of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestConfig :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe SpotFleetRequestConfigData)
sfrcSpotFleetRequestConfig = Lens.lens (spotFleetRequestConfig :: SpotFleetRequestConfig -> Lude.Maybe SpotFleetRequestConfigData) (\s a -> s {spotFleetRequestConfig = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcSpotFleetRequestConfig "Use generic-lens or generic-optics with 'spotFleetRequestConfig' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestId :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe Lude.Text)
sfrcSpotFleetRequestId = Lens.lens (spotFleetRequestId :: SpotFleetRequestConfig -> Lude.Maybe Lude.Text) (\s a -> s {spotFleetRequestId = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The state of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcSpotFleetRequestState :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe BatchState)
sfrcSpotFleetRequestState = Lens.lens (spotFleetRequestState :: SpotFleetRequestConfig -> Lude.Maybe BatchState) (\s a -> s {spotFleetRequestState = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcSpotFleetRequestState "Use generic-lens or generic-optics with 'spotFleetRequestState' instead." #-}

-- | The creation date and time of the request.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcCreateTime :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe Lude.DateTime)
sfrcCreateTime = Lens.lens (createTime :: SpotFleetRequestConfig -> Lude.Maybe Lude.DateTime) (\s a -> s {createTime = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The tags for a Spot Fleet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcTags :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe [Tag])
sfrcTags = Lens.lens (tags :: SpotFleetRequestConfig -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
--
-- /Note:/ Consider using 'activityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcActivityStatus :: Lens.Lens' SpotFleetRequestConfig (Lude.Maybe ActivityStatus)
sfrcActivityStatus = Lens.lens (activityStatus :: SpotFleetRequestConfig -> Lude.Maybe ActivityStatus) (\s a -> s {activityStatus = a} :: SpotFleetRequestConfig)
{-# DEPRECATED sfrcActivityStatus "Use generic-lens or generic-optics with 'activityStatus' instead." #-}

instance Lude.FromXML SpotFleetRequestConfig where
  parseXML x =
    SpotFleetRequestConfig'
      Lude.<$> (x Lude..@? "spotFleetRequestConfig")
      Lude.<*> (x Lude..@? "spotFleetRequestId")
      Lude.<*> (x Lude..@? "spotFleetRequestState")
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "activityStatus")

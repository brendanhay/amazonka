{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDestination
  ( CloudWatchDestination (..),

    -- * Smart constructor
    mkCloudWatchDestination,

    -- * Lenses
    cwdDimensionConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration

-- | Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.
--
-- Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloudWatchDestination' smart constructor.
newtype CloudWatchDestination = CloudWatchDestination'
  { dimensionConfigurations ::
      [CloudWatchDimensionConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchDestination' with the minimum fields required to make a request.
--
-- * 'dimensionConfigurations' - A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
mkCloudWatchDestination ::
  CloudWatchDestination
mkCloudWatchDestination =
  CloudWatchDestination' {dimensionConfigurations = Lude.mempty}

-- | A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
--
-- /Note:/ Consider using 'dimensionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdDimensionConfigurations :: Lens.Lens' CloudWatchDestination [CloudWatchDimensionConfiguration]
cwdDimensionConfigurations = Lens.lens (dimensionConfigurations :: CloudWatchDestination -> [CloudWatchDimensionConfiguration]) (\s a -> s {dimensionConfigurations = a} :: CloudWatchDestination)
{-# DEPRECATED cwdDimensionConfigurations "Use generic-lens or generic-optics with 'dimensionConfigurations' instead." #-}

instance Lude.FromXML CloudWatchDestination where
  parseXML x =
    CloudWatchDestination'
      Lude.<$> ( x Lude..@? "DimensionConfigurations" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "member"
               )

instance Lude.ToQuery CloudWatchDestination where
  toQuery CloudWatchDestination' {..} =
    Lude.mconcat
      [ "DimensionConfigurations"
          Lude.=: Lude.toQueryList "member" dimensionConfigurations
      ]

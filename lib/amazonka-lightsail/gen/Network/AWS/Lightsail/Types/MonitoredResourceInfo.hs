-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MonitoredResourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MonitoredResourceInfo
  ( MonitoredResourceInfo (..),

    -- * Smart constructor
    mkMonitoredResourceInfo,

    -- * Lenses
    mriResourceType,
    mriArn,
    mriName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes resource being monitored by an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
-- /See:/ 'mkMonitoredResourceInfo' smart constructor.
data MonitoredResourceInfo = MonitoredResourceInfo'
  { resourceType ::
      Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoredResourceInfo' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the resource being monitored.
-- * 'name' - The name of the Lightsail resource being monitored.
-- * 'resourceType' - The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
mkMonitoredResourceInfo ::
  MonitoredResourceInfo
mkMonitoredResourceInfo =
  MonitoredResourceInfo'
    { resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriResourceType :: Lens.Lens' MonitoredResourceInfo (Lude.Maybe ResourceType)
mriResourceType = Lens.lens (resourceType :: MonitoredResourceInfo -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: MonitoredResourceInfo)
{-# DEPRECATED mriResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource being monitored.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriArn :: Lens.Lens' MonitoredResourceInfo (Lude.Maybe Lude.Text)
mriArn = Lens.lens (arn :: MonitoredResourceInfo -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: MonitoredResourceInfo)
{-# DEPRECATED mriArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the Lightsail resource being monitored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriName :: Lens.Lens' MonitoredResourceInfo (Lude.Maybe Lude.Text)
mriName = Lens.lens (name :: MonitoredResourceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MonitoredResourceInfo)
{-# DEPRECATED mriName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON MonitoredResourceInfo where
  parseJSON =
    Lude.withObject
      "MonitoredResourceInfo"
      ( \x ->
          MonitoredResourceInfo'
            Lude.<$> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "name")
      )

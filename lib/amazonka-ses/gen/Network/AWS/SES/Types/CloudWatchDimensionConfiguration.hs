{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDimensionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDimensionConfiguration
  ( CloudWatchDimensionConfiguration (..),

    -- * Smart constructor
    mkCloudWatchDimensionConfiguration,

    -- * Lenses
    cwdcDimensionName,
    cwdcDimensionValueSource,
    cwdcDefaultDimensionValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.DefaultDimensionValue as Types
import qualified Network.AWS.SES.Types.DimensionName as Types
import qualified Network.AWS.SES.Types.DimensionValueSource as Types

-- | Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.
--
-- For information about publishing email sending events to Amazon CloudWatch, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { -- | The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain less than 256 characters.
    dimensionName :: Types.DimensionName,
    -- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
    dimensionValueSource :: Types.DimensionValueSource,
    -- | The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain less than 256 characters.
    defaultDimensionValue :: Types.DefaultDimensionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchDimensionConfiguration' value with any optional fields omitted.
mkCloudWatchDimensionConfiguration ::
  -- | 'dimensionName'
  Types.DimensionName ->
  -- | 'dimensionValueSource'
  Types.DimensionValueSource ->
  -- | 'defaultDimensionValue'
  Types.DefaultDimensionValue ->
  CloudWatchDimensionConfiguration
mkCloudWatchDimensionConfiguration
  dimensionName
  dimensionValueSource
  defaultDimensionValue =
    CloudWatchDimensionConfiguration'
      { dimensionName,
        dimensionValueSource,
        defaultDimensionValue
      }

-- | The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'dimensionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdcDimensionName :: Lens.Lens' CloudWatchDimensionConfiguration Types.DimensionName
cwdcDimensionName = Lens.field @"dimensionName"
{-# DEPRECATED cwdcDimensionName "Use generic-lens or generic-optics with 'dimensionName' instead." #-}

-- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
--
-- /Note:/ Consider using 'dimensionValueSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdcDimensionValueSource :: Lens.Lens' CloudWatchDimensionConfiguration Types.DimensionValueSource
cwdcDimensionValueSource = Lens.field @"dimensionValueSource"
{-# DEPRECATED cwdcDimensionValueSource "Use generic-lens or generic-optics with 'dimensionValueSource' instead." #-}

-- | The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'defaultDimensionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdcDefaultDimensionValue :: Lens.Lens' CloudWatchDimensionConfiguration Types.DefaultDimensionValue
cwdcDefaultDimensionValue = Lens.field @"defaultDimensionValue"
{-# DEPRECATED cwdcDefaultDimensionValue "Use generic-lens or generic-optics with 'defaultDimensionValue' instead." #-}

instance Core.FromXML CloudWatchDimensionConfiguration where
  parseXML x =
    CloudWatchDimensionConfiguration'
      Core.<$> (x Core..@ "DimensionName")
      Core.<*> (x Core..@ "DimensionValueSource")
      Core.<*> (x Core..@ "DefaultDimensionValue")

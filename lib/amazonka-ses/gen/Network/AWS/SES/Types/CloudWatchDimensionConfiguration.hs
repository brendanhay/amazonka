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
    cwdcDefaultDimensionValue,
    cwdcDimensionValueSource,
    cwdcDimensionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.DimensionValueSource

-- | Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.
--
-- For information about publishing email sending events to Amazon CloudWatch, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { -- | The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain less than 256 characters.
    defaultDimensionValue :: Lude.Text,
    -- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
    dimensionValueSource :: DimensionValueSource,
    -- | The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain less than 256 characters.
    dimensionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchDimensionConfiguration' with the minimum fields required to make a request.
--
-- * 'defaultDimensionValue' - The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
-- * 'dimensionValueSource' - The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
-- * 'dimensionName' - The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
mkCloudWatchDimensionConfiguration ::
  -- | 'defaultDimensionValue'
  Lude.Text ->
  -- | 'dimensionValueSource'
  DimensionValueSource ->
  -- | 'dimensionName'
  Lude.Text ->
  CloudWatchDimensionConfiguration
mkCloudWatchDimensionConfiguration
  pDefaultDimensionValue_
  pDimensionValueSource_
  pDimensionName_ =
    CloudWatchDimensionConfiguration'
      { defaultDimensionValue =
          pDefaultDimensionValue_,
        dimensionValueSource = pDimensionValueSource_,
        dimensionName = pDimensionName_
      }

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
cwdcDefaultDimensionValue :: Lens.Lens' CloudWatchDimensionConfiguration Lude.Text
cwdcDefaultDimensionValue = Lens.lens (defaultDimensionValue :: CloudWatchDimensionConfiguration -> Lude.Text) (\s a -> s {defaultDimensionValue = a} :: CloudWatchDimensionConfiguration)
{-# DEPRECATED cwdcDefaultDimensionValue "Use generic-lens or generic-optics with 'defaultDimensionValue' instead." #-}

-- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
--
-- /Note:/ Consider using 'dimensionValueSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdcDimensionValueSource :: Lens.Lens' CloudWatchDimensionConfiguration DimensionValueSource
cwdcDimensionValueSource = Lens.lens (dimensionValueSource :: CloudWatchDimensionConfiguration -> DimensionValueSource) (\s a -> s {dimensionValueSource = a} :: CloudWatchDimensionConfiguration)
{-# DEPRECATED cwdcDimensionValueSource "Use generic-lens or generic-optics with 'dimensionValueSource' instead." #-}

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
cwdcDimensionName :: Lens.Lens' CloudWatchDimensionConfiguration Lude.Text
cwdcDimensionName = Lens.lens (dimensionName :: CloudWatchDimensionConfiguration -> Lude.Text) (\s a -> s {dimensionName = a} :: CloudWatchDimensionConfiguration)
{-# DEPRECATED cwdcDimensionName "Use generic-lens or generic-optics with 'dimensionName' instead." #-}

instance Lude.FromXML CloudWatchDimensionConfiguration where
  parseXML x =
    CloudWatchDimensionConfiguration'
      Lude.<$> (x Lude..@ "DefaultDimensionValue")
      Lude.<*> (x Lude..@ "DimensionValueSource")
      Lude.<*> (x Lude..@ "DimensionName")

instance Lude.ToQuery CloudWatchDimensionConfiguration where
  toQuery CloudWatchDimensionConfiguration' {..} =
    Lude.mconcat
      [ "DefaultDimensionValue" Lude.=: defaultDimensionValue,
        "DimensionValueSource" Lude.=: dimensionValueSource,
        "DimensionName" Lude.=: dimensionName
      ]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDimensionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDimensionConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.DimensionValueSource

-- | Contains the dimension configuration to use when you publish email
-- sending events to Amazon CloudWatch.
--
-- For information about publishing email sending events to Amazon
-- CloudWatch, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { -- | The name of an Amazon CloudWatch dimension associated with an email
    -- sending metric. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Contain less than 256 characters.
    dimensionName :: Prelude.Text,
    -- | The place where Amazon SES finds the value of a dimension to publish to
    -- Amazon CloudWatch. If you want Amazon SES to use the message tags that
    -- you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
    -- @SendEmail@\/@SendRawEmail@ API, choose @messageTag@. If you want Amazon
    -- SES to use your own email headers, choose @emailHeader@.
    dimensionValueSource :: DimensionValueSource,
    -- | The default value of the dimension that is published to Amazon
    -- CloudWatch if you do not provide the value of the dimension when you
    -- send an email. The default value must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Contain less than 256 characters.
    defaultDimensionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchDimensionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'cloudWatchDimensionConfiguration_dimensionName' - The name of an Amazon CloudWatch dimension associated with an email
-- sending metric. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
--
-- 'dimensionValueSource', 'cloudWatchDimensionConfiguration_dimensionValueSource' - The place where Amazon SES finds the value of a dimension to publish to
-- Amazon CloudWatch. If you want Amazon SES to use the message tags that
-- you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
-- @SendEmail@\/@SendRawEmail@ API, choose @messageTag@. If you want Amazon
-- SES to use your own email headers, choose @emailHeader@.
--
-- 'defaultDimensionValue', 'cloudWatchDimensionConfiguration_defaultDimensionValue' - The default value of the dimension that is published to Amazon
-- CloudWatch if you do not provide the value of the dimension when you
-- send an email. The default value must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
newCloudWatchDimensionConfiguration ::
  -- | 'dimensionName'
  Prelude.Text ->
  -- | 'dimensionValueSource'
  DimensionValueSource ->
  -- | 'defaultDimensionValue'
  Prelude.Text ->
  CloudWatchDimensionConfiguration
newCloudWatchDimensionConfiguration
  pDimensionName_
  pDimensionValueSource_
  pDefaultDimensionValue_ =
    CloudWatchDimensionConfiguration'
      { dimensionName =
          pDimensionName_,
        dimensionValueSource =
          pDimensionValueSource_,
        defaultDimensionValue =
          pDefaultDimensionValue_
      }

-- | The name of an Amazon CloudWatch dimension associated with an email
-- sending metric. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
cloudWatchDimensionConfiguration_dimensionName :: Lens.Lens' CloudWatchDimensionConfiguration Prelude.Text
cloudWatchDimensionConfiguration_dimensionName = Lens.lens (\CloudWatchDimensionConfiguration' {dimensionName} -> dimensionName) (\s@CloudWatchDimensionConfiguration' {} a -> s {dimensionName = a} :: CloudWatchDimensionConfiguration)

-- | The place where Amazon SES finds the value of a dimension to publish to
-- Amazon CloudWatch. If you want Amazon SES to use the message tags that
-- you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the
-- @SendEmail@\/@SendRawEmail@ API, choose @messageTag@. If you want Amazon
-- SES to use your own email headers, choose @emailHeader@.
cloudWatchDimensionConfiguration_dimensionValueSource :: Lens.Lens' CloudWatchDimensionConfiguration DimensionValueSource
cloudWatchDimensionConfiguration_dimensionValueSource = Lens.lens (\CloudWatchDimensionConfiguration' {dimensionValueSource} -> dimensionValueSource) (\s@CloudWatchDimensionConfiguration' {} a -> s {dimensionValueSource = a} :: CloudWatchDimensionConfiguration)

-- | The default value of the dimension that is published to Amazon
-- CloudWatch if you do not provide the value of the dimension when you
-- send an email. The default value must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 256 characters.
cloudWatchDimensionConfiguration_defaultDimensionValue :: Lens.Lens' CloudWatchDimensionConfiguration Prelude.Text
cloudWatchDimensionConfiguration_defaultDimensionValue = Lens.lens (\CloudWatchDimensionConfiguration' {defaultDimensionValue} -> defaultDimensionValue) (\s@CloudWatchDimensionConfiguration' {} a -> s {defaultDimensionValue = a} :: CloudWatchDimensionConfiguration)

instance
  Prelude.FromXML
    CloudWatchDimensionConfiguration
  where
  parseXML x =
    CloudWatchDimensionConfiguration'
      Prelude.<$> (x Prelude..@ "DimensionName")
      Prelude.<*> (x Prelude..@ "DimensionValueSource")
      Prelude.<*> (x Prelude..@ "DefaultDimensionValue")

instance
  Prelude.Hashable
    CloudWatchDimensionConfiguration

instance
  Prelude.NFData
    CloudWatchDimensionConfiguration

instance
  Prelude.ToQuery
    CloudWatchDimensionConfiguration
  where
  toQuery CloudWatchDimensionConfiguration' {..} =
    Prelude.mconcat
      [ "DimensionName" Prelude.=: dimensionName,
        "DimensionValueSource"
          Prelude.=: dimensionValueSource,
        "DefaultDimensionValue"
          Prelude.=: defaultDimensionValue
      ]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDimensionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDimensionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.DimensionValueSource

-- | Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.
--
--
-- For information about publishing email sending events to Amazon CloudWatch, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'cloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { _cwdcDimensionName ::
      !Text,
    _cwdcDimensionValueSource ::
      !DimensionValueSource,
    _cwdcDefaultDimensionValue ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchDimensionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdcDimensionName' - The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
--
-- * 'cwdcDimensionValueSource' - The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
--
-- * 'cwdcDefaultDimensionValue' - The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cloudWatchDimensionConfiguration ::
  -- | 'cwdcDimensionName'
  Text ->
  -- | 'cwdcDimensionValueSource'
  DimensionValueSource ->
  -- | 'cwdcDefaultDimensionValue'
  Text ->
  CloudWatchDimensionConfiguration
cloudWatchDimensionConfiguration
  pDimensionName_
  pDimensionValueSource_
  pDefaultDimensionValue_ =
    CloudWatchDimensionConfiguration'
      { _cwdcDimensionName =
          pDimensionName_,
        _cwdcDimensionValueSource = pDimensionValueSource_,
        _cwdcDefaultDimensionValue = pDefaultDimensionValue_
      }

-- | The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cwdcDimensionName :: Lens' CloudWatchDimensionConfiguration Text
cwdcDimensionName = lens _cwdcDimensionName (\s a -> s {_cwdcDimensionName = a})

-- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
cwdcDimensionValueSource :: Lens' CloudWatchDimensionConfiguration DimensionValueSource
cwdcDimensionValueSource = lens _cwdcDimensionValueSource (\s a -> s {_cwdcDimensionValueSource = a})

-- | The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cwdcDefaultDimensionValue :: Lens' CloudWatchDimensionConfiguration Text
cwdcDefaultDimensionValue = lens _cwdcDefaultDimensionValue (\s a -> s {_cwdcDefaultDimensionValue = a})

instance FromXML CloudWatchDimensionConfiguration where
  parseXML x =
    CloudWatchDimensionConfiguration'
      <$> (x .@ "DimensionName")
      <*> (x .@ "DimensionValueSource")
      <*> (x .@ "DefaultDimensionValue")

instance Hashable CloudWatchDimensionConfiguration

instance NFData CloudWatchDimensionConfiguration

instance ToQuery CloudWatchDimensionConfiguration where
  toQuery CloudWatchDimensionConfiguration' {..} =
    mconcat
      [ "DimensionName" =: _cwdcDimensionName,
        "DimensionValueSource" =: _cwdcDimensionValueSource,
        "DefaultDimensionValue" =: _cwdcDefaultDimensionValue
      ]

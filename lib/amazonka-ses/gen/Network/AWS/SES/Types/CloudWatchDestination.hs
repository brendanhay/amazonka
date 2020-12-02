{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration

-- | Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.
--
--
-- Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'cloudWatchDestination' smart constructor.
newtype CloudWatchDestination = CloudWatchDestination'
  { _cwdDimensionConfigurations ::
      [CloudWatchDimensionConfiguration]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdDimensionConfigurations' - A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
cloudWatchDestination ::
  CloudWatchDestination
cloudWatchDestination =
  CloudWatchDestination' {_cwdDimensionConfigurations = mempty}

-- | A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
cwdDimensionConfigurations :: Lens' CloudWatchDestination [CloudWatchDimensionConfiguration]
cwdDimensionConfigurations = lens _cwdDimensionConfigurations (\s a -> s {_cwdDimensionConfigurations = a}) . _Coerce

instance FromXML CloudWatchDestination where
  parseXML x =
    CloudWatchDestination'
      <$> ( x .@? "DimensionConfigurations" .!@ mempty
              >>= parseXMLList "member"
          )

instance Hashable CloudWatchDestination

instance NFData CloudWatchDestination

instance ToQuery CloudWatchDestination where
  toQuery CloudWatchDestination' {..} =
    mconcat
      [ "DimensionConfigurations"
          =: toQueryList "member" _cwdDimensionConfigurations
      ]

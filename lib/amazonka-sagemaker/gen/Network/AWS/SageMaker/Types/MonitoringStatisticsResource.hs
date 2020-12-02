{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStatisticsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStatisticsResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The statistics resource for a monitoring job.
--
--
--
-- /See:/ 'monitoringStatisticsResource' smart constructor.
newtype MonitoringStatisticsResource = MonitoringStatisticsResource'
  { _msrS3URI ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringStatisticsResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msrS3URI' - The Amazon S3 URI for the statistics resource.
monitoringStatisticsResource ::
  MonitoringStatisticsResource
monitoringStatisticsResource =
  MonitoringStatisticsResource' {_msrS3URI = Nothing}

-- | The Amazon S3 URI for the statistics resource.
msrS3URI :: Lens' MonitoringStatisticsResource (Maybe Text)
msrS3URI = lens _msrS3URI (\s a -> s {_msrS3URI = a})

instance FromJSON MonitoringStatisticsResource where
  parseJSON =
    withObject
      "MonitoringStatisticsResource"
      (\x -> MonitoringStatisticsResource' <$> (x .:? "S3Uri"))

instance Hashable MonitoringStatisticsResource

instance NFData MonitoringStatisticsResource

instance ToJSON MonitoringStatisticsResource where
  toJSON MonitoringStatisticsResource' {..} =
    object (catMaybes [("S3Uri" .=) <$> _msrS3URI])

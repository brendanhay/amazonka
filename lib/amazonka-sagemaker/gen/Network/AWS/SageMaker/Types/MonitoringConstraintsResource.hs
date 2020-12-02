{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringConstraintsResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The constraints resource for a monitoring job.
--
--
--
-- /See:/ 'monitoringConstraintsResource' smart constructor.
newtype MonitoringConstraintsResource = MonitoringConstraintsResource'
  { _mcrS3URI ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringConstraintsResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrS3URI' - The Amazon S3 URI for the constraints resource.
monitoringConstraintsResource ::
  MonitoringConstraintsResource
monitoringConstraintsResource =
  MonitoringConstraintsResource' {_mcrS3URI = Nothing}

-- | The Amazon S3 URI for the constraints resource.
mcrS3URI :: Lens' MonitoringConstraintsResource (Maybe Text)
mcrS3URI = lens _mcrS3URI (\s a -> s {_mcrS3URI = a})

instance FromJSON MonitoringConstraintsResource where
  parseJSON =
    withObject
      "MonitoringConstraintsResource"
      (\x -> MonitoringConstraintsResource' <$> (x .:? "S3Uri"))

instance Hashable MonitoringConstraintsResource

instance NFData MonitoringConstraintsResource

instance ToJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource' {..} =
    object (catMaybes [("S3Uri" .=) <$> _mcrS3URI])

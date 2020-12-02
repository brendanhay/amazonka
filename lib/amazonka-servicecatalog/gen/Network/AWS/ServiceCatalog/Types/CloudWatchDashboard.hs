{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CloudWatchDashboard where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a CloudWatch dashboard.
--
--
--
-- /See:/ 'cloudWatchDashboard' smart constructor.
newtype CloudWatchDashboard = CloudWatchDashboard'
  { _cwdName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchDashboard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdName' - The name of the CloudWatch dashboard.
cloudWatchDashboard ::
  CloudWatchDashboard
cloudWatchDashboard = CloudWatchDashboard' {_cwdName = Nothing}

-- | The name of the CloudWatch dashboard.
cwdName :: Lens' CloudWatchDashboard (Maybe Text)
cwdName = lens _cwdName (\s a -> s {_cwdName = a})

instance FromJSON CloudWatchDashboard where
  parseJSON =
    withObject
      "CloudWatchDashboard"
      (\x -> CloudWatchDashboard' <$> (x .:? "Name"))

instance Hashable CloudWatchDashboard

instance NFData CloudWatchDashboard
